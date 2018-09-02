defmodule ExDot do
  def from_string(string) do
    ExDot.Parser.parse(string) |> create_graph
  end

  def from_file(file) do
    case File.read(file) do
      {:ok, string} -> ExDot.Parser.parse(string) |> create_graph
      error -> error
    end
  end

  defp create_graph({:ok, %{statements: statements}}) do
    graph = Graph.new()

    graph =
      statements
      |> Stream.filter(fn
        %{type: "node", name: _} -> true
        _ -> false
      end)
      |> Enum.reduce(graph, &create_vertex/2)

    statements
    |> Stream.filter(fn
      %{type: "edge", from: _} -> true
      _ -> false
    end)
    |> Stream.map(&create_edge/1)
    |> Enum.reduce(graph, &Graph.add_edge(&2, &1))
  end

  defp create_graph(error), do: error

  defp create_vertex(%{name: name, attributes: attributes}, graph) do
    graph |> Graph.add_vertex(name, [attributes])
  end

  defp create_edge(%{from: from, to: to, attributes: attributes}) do
    Graph.Edge.new(from, to, label: attributes)
  end
end
