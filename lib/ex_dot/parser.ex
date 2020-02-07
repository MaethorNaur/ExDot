defmodule ExDot.Parser do
  defstruct [:input, line: 0, position: 0]
  use ExDot.Parser.Combinator

  def parse(input), do: graph().(input)

  defp graph,
    do:
      newline()
      ~> optional("strict" ~> whitespaces())
      ~> ("graph" | "digraph")
      ~> (optional(whitespaces() ~> (identifier() | string()))
          ~>> fn
            nil -> nil
            [_, identifier] -> identifier
          end)
      ~> optional(whitespaces())
      ~> '{'
      ~> newline()
      ~> repeat(statements())
      ~> '}'
      ~> newline()
      ~>> fn [_, strict, graph, name, _, _, _, statements, _, _] ->
        %{
          strict: !is_nil(strict),
          mode: graph,
          name: name,
          statements: statements |> Enum.flat_map(& &1)
        }
      end

  defp statements,
    do:
      statement()
      ~> optional(whitespaces())
      ~> maybe(';')
      ~> newline()
      ~>> (&List.first/1)
      |> named("statements")

  defp statement,
    do:
      (id_statement()
       | edge_statement()
       | node_statement()
       | attribute_statement()
       | subgraph())
      |> named("statement")

  defp subgraph,
    do:
      optional(
        "subgraph"
        ~> whitespaces()
        ~> (identifier() | string())
        ~> optional(whitespaces())
      )
      ~> '{'
      ~> newline()
      ~> lazy(fn -> repeat(statements()) end)
      ~> '}'
      ~>> fn list -> list |> Enum.at(3) |> Enum.flat_map(& &1) end
      |> named("subgraph")

  defp id_statement,
    do: key_value() ~>> (&[%{type: "data", attributes: &1}]) |> named("id_statement")

  defp edge_statement,
    do:
      (node_id() | subgraph())
      ~> optional(whitespaces())
      ~> edge_op()
      ~> optional(whitespaces())
      ~> (node_id() | subgraph())
      ~> optional(whitespaces())
      ~> repeat(attribute_list())
      ~>> fn [from, _, mode, _, to, _, attributes] ->
        attributes = create_attribute_list(attributes)

        to =
          to
          |> Enum.reject(&is_nil/1)
          |> Enum.map(fn
            %{name: name} -> name
            name -> name
          end)

        from
        |> Enum.reject(&is_nil/1)
        |> Enum.map(fn
          %{name: name} -> name
          name -> name
        end)
        |> Enum.flat_map(fn from ->
          to
          |> Enum.map(fn to ->
            %{
              type: "edge",
              mode: mode,
              from: from,
              to: to,
              attributes: attributes
            }
          end)
        end)
      end
      |> named("edge_statement")

  defp node_statement,
    do:
      node_id()
      ~> repeat(attribute_list())
      ~>> fn [node_id, attributes] ->
        [
          %{
            type: "node",
            name: hd(node_id),
            attributes: create_attribute_list(attributes)
          }
        ]
      end
      |> named("node_statement")

  defp attribute_statement,
    do:
      ("graph" | "node" | "edge")
      ~> repeat(attribute_list(), 1)
      ~>> fn [type, attributes] ->
        [%{type: type, attributes: create_attribute_list(attributes)}]
      end
      |> named("attribute_statement")

  defp node_id,
    do:
      satisfy(identifier(), &(not (&1 in ["node", "edge", "graph"])))
      ~> optional(port())
      |> named("node_id")

  defp edge_op, do: "--" | "->"

  def port,
    do:
      optional(whitespaces())
      ~> ':'
      ~> optional(whitespaces())
      ~> (compass_pt() | identifier() ~> optional(maybe(':') ~> compass_pt()))
      ~>> fn
        [_, _, _, [direction]] -> [direction: direction, id: nil]
        [_, _, _, [id, nil]] -> [direction: nil, id: id]
        [_, _, _, [id, [_, direction]]] -> [direction: direction, id: id]
      end
      |> named("port")

  defp compass_pt,
    do:
      optional(whitespaces())
      ~> maybe(':')
      ~> optional(whitespaces())
      ~> (token(:ne)
          | token(:nw)
          | token(:n)
          | token(:se)
          | token(:sw)
          | token(:s)
          | token(:e)
          | token(:w)
          | token(:c)
          | token(:_))
      ~>> (&List.last/1)
      |> named("compass_pt")

  defp attribute_list,
    do:
      optional(whitespaces())
      ~> '['
      ~> optional(whitespaces())
      ~> repeat(list())
      ~> ']'
      ~> optional(whitespaces())
      ~>> (&Enum.at(&1, 3))
      |> named("attribute_list")

  defp list,
    do:
      key_value()
      ~> optional(whitespaces())
      ~> optional(';' | ',')
      ~> optional(whitespaces())
      ~>> (&List.first/1)
      |> named("list")

  defp key_value,
    do:
      identifier()
      ~> optional(whitespaces())
      ~> '='
      ~> optional(whitespaces())
      ~> (identifier() | string())
      ~>> fn list -> {List.first(list), List.last(list)} end
      |> named("key_value")

  defp create_attribute_list(attributes) do
    attributes
    |> Enum.flat_map(& &1)
    |> Enum.into(%{})
  end

  defp token(expected),
    do:
      identifier()
      |> satisfy(fn found -> String.upcase(found) == String.upcase(to_string(expected)) end)
      |> map(fn _ -> expected end)
      |> named(to_string(expected))

  defp identifier,
    do:
      ((printable_letter() | '_') ~> repeat(identifier_char()) ~>> (&concat/1)
       | maybe('-')
         ~> ('.' ~> repeat(digit(), 1) ~>> (&concat/1)
             | repeat(digit(), 1)
               ~> optional(
                 maybe('.')
                 ~> repeat(digit(), 1)
                 ~>> (&concat/1)
               )
               ~>> (&concat/1))
         ~>> (&concat/1))
      ~>> (&to_string/1)
      |> named("identifier")

  defp concat([nil, right]), do: right
  defp concat([left, nil]), do: left
  defp concat([left, right]) when is_list(left), do: left ++ right
  defp concat([left, right]), do: [left | right]

  defp comment, do: line_comment() | multiline_comment()

  defp line_comment,
    do: ("//" | '#') ~> repeat(satisfy(any(), fn char -> not (char in [?\n, ?\r]) end))

  defp multiline_comment,
    do:
      "/*"
      ~> repeat_until(any(), fn acc ->
        case Enum.reverse(acc) do
          [?/, ?* | _] ->
            false

          _ ->
            true
        end
      end)

  defp newline, do: repeat(comment() | ' ' | '\r' | '\n' | '\s' | '\t')

  defp whitespaces, do: repeat(comment() | ' ' | '\s' | '\t', 1)

  defp identifier_char, do: choice([printable_letter(), char(?_), digit()])

  def digit, do: satisfy(any(), fn char -> char in ?0..?9 end)

  def printable_letter,
    do: satisfy(any(), fn char -> char in ?A..?Z or char in ?a..?z or char in ?È..?Ź end)

  defp string,
    do: '"' ~> repeat(double_string()) ~> '"' ~>> (&(&1 |> Enum.at(1) |> to_string()))

  defp double_string,
    do:
      '\\' ~> escape_sequence() ~>> (&Enum.at(&1, 1)) | satisfy(any(), fn char -> char != ?" end)

  defp escape_sequence,
    do:
      ('"' | '\\' | 'b' | 'f' | 'n' | 'r' | 's' | 't' | 'v')
      ~>> fn
        ?b -> ?\b
        ?f -> ?\f
        ?n -> ?\n
        ?r -> ?\r
        ?s -> ?\s
        ?t -> ?\t
        ?v -> ?\v
        value -> value
      end

  def keyword(expected),
    do:
      identifier()
      |> token()
      |> satisfy(fn identifier ->
        String.upcase(identifier) == String.upcase(to_string(expected))
      end)
      |> map(fn _ -> expected end)
end
