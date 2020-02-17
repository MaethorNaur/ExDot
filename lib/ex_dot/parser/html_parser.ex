defmodule ExDot.Parser.HTMLParser do
  defmodule Tag do
    import Kernel, except: [to_string: 1]
    defstruct [:tag, inner: [], attributes: []]

    defimpl String.Chars, for: Tag do
      def to_string(%{tag: tag, attributes: attributes, inner: inner}) do
        attributes =
          if not Enum.empty?(attributes) do
            " " <>
              (attributes
               |> Enum.map(fn {k, v} ->
                 "#{k}=\"#{String.replace(v, ["\"", "'"], fn _ -> "\"" end)}\""
               end)
               |> Enum.join(" "))
          else
            ""
          end

        case inner do
          [] ->
            "<#{tag}#{attributes}/>"

          _ ->
            inner = inner |> Enum.map(&Kernel.to_string/1) |> Enum.join(" ")
            "<#{tag}#{attributes}>#{inner}</#{tag}>"
        end
      end
    end
  end

  use ExDot.Parser.Combinator

  def start_link do
    with {:error, _} <- Agent.start_link(fn -> [] end, name: __MODULE__) do
      stop()
      Agent.start_link(fn -> [] end, name: __MODULE__)
    end
  end

  def stop, do: Agent.stop(__MODULE__)
  def tag, do: inline_tag() | opening_closing_tag()

  defp inline_tag,
    do:
      '<'
      ~> tag_attributes()
      ~> '/'
      ~> '>'
      ~>> (&Enum.at(&1, 1))

  defp tag_attributes(push \\ false),
    do:
      whitespaces()
      ~> if(push, do: push(identifier()), else: identifier())
      ~> whitespaces()
      ~> (repeat(attributes()) ~>> (&Enum.into(&1, %{})))
      ~> whitespaces()
      ~>> fn [_, tag, _, attributes, _] -> %Tag{tag: tag, inner: [], attributes: attributes} end

  defp attributes,
    do:
      whitespaces()
      ~> identifier()
      ~> whitespaces()
      ~> '='
      ~> whitespaces()
      ~> (identifier() | string(?") | string(?'))
      ~>> fn [_, key, _, _, _, value] -> {key, value} end

  defp string(quote_type),
    do:
      char(quote_type)
      ~> repeat(double_string(quote_type))
      ~> char(quote_type)
      ~>> (&(&1 |> Enum.at(1) |> to_string()))

  defp double_string(quote_type),
    do:
      ('\\' ~> escape_sequence(quote_type) ~>> (&Enum.at(&1, 1))
       | satisfy(any(), fn char -> char != quote_type end))
      |> named("Quote string: #{[quote_type]}")

  defp escape_sequence(quote_type),
    do:
      (char(quote_type) | '\\' | 'b' | 'f' | 'n' | 'r' | 's' | 't' | 'v')
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

  defp opening_closing_tag,
    do:
      '<'
      ~> tag_attributes(true)
      ~> '>'
      ~> whitespaces()
      ~> (repeat(lazy(fn -> satisfy(any(), &(&1 != ?<)) | tag() end))
          ~>> fn list ->
            {acc, str} =
              Enum.reduce(list, {[], []}, fn
                %Tag{} = inner, {acc, [_ | _] = str} ->
                  {[inner, to_string(Enum.reverse(str)) | acc], []}

                %Tag{} = inner, {acc, str} ->
                  {[inner | acc], str}

                char, {acc, str} ->
                  {acc, [char | str]}
              end)

            if str == [] do
              acc
            else
              [to_string(Enum.reverse(str)) | acc]
            end
            |> Enum.reverse()
          end)
      ~> whitespaces()
      ~> "</"
      ~> whitespaces()
      ~> pop(identifier())
      ~> whitespaces()
      ~> '>'
      ~>> fn [_, tag, _, _, inner, _, _, _, _, _, _] ->
        %Tag{tag | inner: inner}
      end

  defp whitespaces, do: repeat(' ' | '\r' | '\n' | '\s' | '\t')

  defp push(parser),
    do: fn input ->
      with {:ok, term, rest} <- parser.(input) do
        Agent.update(__MODULE__, &[term | &1])
        {:ok, term, rest}
      end
    end

  defp pop(parser),
    do: fn input ->
      case Agent.get_and_update(__MODULE__, fn stack -> {List.first(stack), tl(stack)} end) do
        nil ->
          {:error, "pop without stack"}

        last ->
          case parser.(input) do
            {:ok, ^last, rest} -> {:ok, last, rest}
            {:ok, term, _} -> {:error, "#{inspect(term)} != #{inspect(last)}"}
            {:error, reason} -> {:error, reason}
          end
      end
    end

  defp identifier,
    do:
      repeat(identifier_char(), 1)
      ~>> (&to_string/1)
      |> named("identifier")

  defp identifier_char, do: printable_letter() | '_' | '-' | digit()

  defp digit, do: satisfy(any(), fn char -> char in ?0..?9 end)

  def printable_letter,
    do: satisfy(any(), fn char -> char in ?A..?Z or char in ?a..?z or char in ?È..?Ź end)
end
