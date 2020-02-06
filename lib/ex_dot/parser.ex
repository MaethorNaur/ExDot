defmodule ExDot.Parser do
  defstruct [:input, line: 0, position: 0]
  use ExDot.Parser.Combinator

  def parse(input), do: graph().(input)

  defp graph,
    do:
      optional(whitespaces())
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
      ~>> fn [_, strict, graph, name, _, _, _, statements, _] ->
        %{
          strict: !is_nil(strict),
          mode: graph,
          name: name,
          statements: statements |> Enum.flat_map(& &1)
        }
      end

  defp statements,
    do: statement() ~> optional(whitespaces()) ~> maybe(';') ~> newline() ~>> (&Enum.at(&1, 0))

  defp statement,
    do: subgraph() | id_statement() | edge_statement() | node_statement() | attribute_statement()

  defp subgraph,
    do:
      "subgraph"
      ~> (optional(whitespaces() ~> (identifier() | string()))
          ~>> fn
            nil -> nil
            [_, identifier] -> identifier
          end)
      ~> optional(whitespaces())
      ~> '{'
      ~> newline()
      ~> repeat(lazy(fn -> statements() end))
      ~> '}'
      ~>> fn list -> list |> Enum.at(5) |> Enum.flat_map(& &1) end

  defp id_statement, do: key_value() ~>> (&[%{type: "data", attributes: &1}])

  defp edge_statement,
    do:
      node_id()
      ~> optional(whitespaces())
      ~> edge_op()
      ~> optional(whitespaces())
      ~> node_id()
      ~> optional(whitespaces())
      ~> repeat(attribute_list())
      ~>> fn [from, _, mode, _, to, _, attributes] ->
        [
          %{
            type: "edge",
            mode: mode,
            from: hd(from),
            to: hd(to),
            attributes: create_attribute_list(attributes)
          }
        ]
      end

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

  defp attribute_statement,
    do:
      ("graph" | "node" | "edge")
      ~> repeat(attribute_list(), 1)
      ~>> fn [type, attributes] ->
        [%{type: type, attributes: create_attribute_list(attributes)}]
      end

  defp node_id,
    do: satisfy(identifier(), &(not (&1 in ["node", "edge", "graph"]))) ~> optional(port())

  defp edge_op, do: "--" | "->"

  def port,
    do:
      optional(whitespaces())
      ~> maybe(':')
      ~> optional(whitespaces())
      ~> (compass_pt() | identifier() ~> optional(compass_pt()))
      ~>> fn
        [_, _, _, [direction]] -> [direction: direction, id: nil]
        [_, _, _, [id, direction]] -> [direction: direction, id: id]
      end

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

  defp attribute_list,
    do:
      optional(whitespaces())
      ~> '['
      ~> optional(whitespaces())
      ~> repeat(list())
      ~> ']'
      ~> optional(whitespaces())
      ~>> (&Enum.at(&1, 3))

  defp list,
    do:
      key_value()
      ~> optional(whitespaces())
      ~> optional(';' | ',')
      ~> optional(whitespaces())
      ~>> (&List.first/1)

  defp key_value,
    do:
      identifier()
      ~> optional(whitespaces())
      ~> '='
      ~> optional(whitespaces())
      ~> (identifier() | string())
      ~>> fn list -> {List.first(list), List.last(list)} end

  defp create_attribute_list(attributes) do
    attributes
    |> Enum.flat_map(& &1)
    |> Enum.into(%{})
  end

  defp token(expected),
    do:
      identifier()
      |> satisfy(fn found -> String.upcase(found) == String.upcase(expected) end)
      |> map(fn _ -> expected end)

  defp identifier,
    do:
      identifier_char()
      |> repeat(1)
      |> satisfy(&match?([_ | _], &1))
      |> map(&to_string/1)

  defp newline, do: repeat(' ' | '\r' | '\n' | '\s' | '\t')
  defp whitespaces, do: repeat(' ' | '\s' | '\t', 1)

  defp identifier_char, do: choice([ascii_letter(), char(?_), digit()])

  def digit, do: satisfy(any(), fn char -> char in ?0..?9 end)

  def ascii_letter, do: satisfy(any(), fn char -> char in ?A..?Z or char in ?a..?z end)

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
