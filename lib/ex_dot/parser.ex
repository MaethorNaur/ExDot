defmodule ExDot.Parser do
  use Neotomex.ExGrammar

  @root true
  define :graph,
         "'strict'? <space?> ('graph' / 'digraph') <space?> (ID / string)? <space?> <'{'> <newline?> statements* <'}'>" do
    [strict, graph, name, statements] ->
      %{
        strict: !is_nil(strict),
        mode: graph,
        name: name,
        statements: statements |> Enum.flat_map(fn v -> v end)
      }
  end

  define :statements, "statement <space?> <';'?> <newline?>" do
    [l] -> l
  end

  define(
    :statement,
    "( id_statement / edge_statement / node_statement / attribute_statement)"
  )

  define(:comment, "'/*' (!'*/' <all>)* '*/'")

  define :edge_statement, "node_id <space?> edge_op <space?> node_id <space?> attribute_list*" do
    [from, mode, to, attributes] ->
      [
        %{
          type: "edge",
          mode: mode,
          from: from |> hd,
          to: to |> hd,
          attributes: attributes |> create_attribute_list
        }
      ]
  end

  define(:edge_op, "'--' / '->'")

  define :node_statement, "node_id <space?> attribute_list*" do
    [l, attributes] ->
      [%{type: "node", name: l |> hd, attributes: attributes |> create_attribute_list}]
  end

  define(:node_id, "<!reserved_keywords> ID port?")

  define :port, "<space?> <':'?> <space?> (compass_pt / (ID compass_pt?))" do
    [[id, [direction]]] -> [direction: direction, id: id]
    [[direction]] -> [direction: direction, id: nil]
  end

  define(
    :compass_pt,
    "<space?> <':'?> <space?> ('ne' / 'nw' / 'n'/ 'se' / 'sw' / 's' / 'e' / 'w' / 'c' / '_')"
  )

  define :attribute_statement, "('graph' / 'node' / 'edge') <space?> attribute_list+" do
    [type, attributes] -> [%{type: type, attributes: attributes |> create_attribute_list}]
  end

  define :id_statement, "key_value" do
    [kv] -> [%{type: "data", attributes: kv}]
  end

  define :attribute_list, "<space?> <'['> <space?>  list* <']'> <space?>" do
    [l] -> l
  end

  define(:list, "key_value <space?> <(';' / ',')?> <space?>") do
    [l] -> l
  end

  define :key_value, "ID <space?> <'='> <space?> ( ID / string)" do
    [key, value] -> [{key, value}]
  end

  define :string, "<'\"'> (double_string)* <'\"'>" do
    [chars] -> Enum.join(chars)
  end

  define(:double_string, "(<!('\"' / '\\\\')> <all>) / (<'\\\\'> escape_sequence)")

  define :escape_sequence, "'\"' / '\\\\' / 'b' / 'f' / 'n' / 'r' / 't' / 'v'" do
    "b" -> "\b"
    "f" -> "\f"
    "n" -> "\n"
    "r" -> "\r"
    "t" -> "\t"
    "v" -> "\v"
    value -> value
  end

  define :ID, "[a-zA-Z][a-zA-Z0-9_]*" do
    list -> Enum.join(list)
  end

  define(:reserved_keywords, "('node' / 'edge' / 'graph')")
  define(:space, "([ \\s\\t] / comment)*")
  define(:all, "([\\r\\n] / .)")
  define(:newline, "([ \\r\\n\\s\\t] / comment)*")

  defp create_attribute_list(attributes) do
    attributes
    |> Enum.flat_map(fn value -> Enum.reduce(value, [], &(&1 ++ &2)) end)
    |> Enum.into(%{})
  end
end
