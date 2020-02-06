defmodule ExDot.Parser.Combinator do
  @type success :: {:ok, term(), binary()}
  @type error :: {:error, term()}
  @type result :: success() | error()
  @type parser :: (binary() -> result())

  defmacro __using__(_) do
    quote location: :keep do
      import ExDot.Parser.Combinator
    end
  end

  defmacro lhs ~> rhs do
    sequence = to_list_sequence(lhs, rhs)
    quote do: sequence(unquote(sequence))
  end

  defmacro lhs ~>> rhs do
    lhs = expand_macro(lhs)
    quote do: map(unquote(lhs), unquote(rhs))
  end

  defmacro maybe(rhs), do: quote(do: optional(unquote(expand_macro(rhs))))

  defmacro lhs | rhs do
    list = to_list_choice(rhs, lhs) |> Enum.reverse()
    quote do: choice(unquote(list))
  end

  defp to_list_sequence(lhs, rhs, acc \\ [])

  defp to_list_sequence({:~>, _, [lhs, rhs]}, term, acc),
    do: to_list_sequence(lhs, rhs, [expand_macro(term) | acc])

  defp to_list_sequence(lhs, rhs, acc),
    do: [expand_macro(lhs), expand_macro(rhs) | acc]

  defp to_list_choice(lhs, rhs, acc \\ [])

  defp to_list_choice(term, {:|, _, [lhs, rhs]}, acc),
    do: to_list_choice(lhs, rhs, [expand_macro(term) | acc])

  defp to_list_choice(lhs, rhs, acc),
    do: [expand_macro(lhs), expand_macro(rhs) | acc]

  defp expand_macro({atom, _, _} = expand) when atom in [:|, :~>],
    do: Macro.expand(expand, __ENV__)

  defp expand_macro(input) when is_binary(input), do: quote(do: value(unquote(input)))
  defp expand_macro(input) when is_integer(input), do: quote(do: char(unquote(input)))
  defp expand_macro([input]) when is_integer(input), do: quote(do: char(unquote(input)))
  defp expand_macro(input), do: input

  @spec optional(parser :: parser()) :: result()
  def optional(parser),
    do: fn input ->
      with {:error, _reason} <- parser.(input) do
        {:ok, nil, input}
      end
    end

  @spec lazy(combinator :: (() -> parser())) :: result()
  def lazy(combinator),
    do: fn input ->
      parser = combinator.()
      parser.(input)
    end

  @spec satisfy(parser :: parser(), predicate :: (term() -> boolean())) :: result()
  def satisfy(parser, predicate),
    do: fn input ->
      with {:ok, term, rest} <- parser.(input) do
        if predicate.(term), do: {:ok, term, rest}, else: {:error, "term unsatisfy #{term}"}
      end
    end

  @spec repeat(parser :: parser, min :: pos_integer, max :: pos_integer) ::
          result()
  def repeat(parser, min \\ 0, max \\ 0) when min >= 0 and max >= 0,
    do: repeat(parser, min, max, 0)

  @spec repeat(parser :: parser, min :: pos_integer, max :: pos_integer, counter :: pos_integer) ::
          result()
  defp repeat(parser, min, max, counter),
    do: fn input ->
      case parser.(input) do
        {:error, _reason} when min > 0 and min > counter ->
          {:error, "Minimum #{min} of occurrences required. Got #{counter}"}

        {:error, _reason} ->
          {:ok, [], input}

        {:ok, first_term, rest} when max > 0 and counter == max - 1 ->
          {:ok, [first_term], rest}

        {:ok, first_term, rest} ->
          {:ok, other_terms, rest} = repeat(parser, min, max, counter + 1).(rest)
          {:ok, [first_term | other_terms], rest}
      end
    end

  @spec choice(parsers :: [parser()]) :: result()
  def choice(parsers),
    do: fn input ->
      case parsers do
        [] -> {:error, "no parser succeed"}
        [head | tail] -> with {:error, _reason} <- head.(input), do: choice(tail).(input)
      end
    end

  @spec map(parser :: parser, mapper :: (term() -> term())) :: parser()
  def map(parser, mapper),
    do: fn input -> with {:ok, term, rest} <- parser.(input), do: {:ok, mapper.(term), rest} end

  @spec sequence(parsers :: [parser()]) :: parser()
  def sequence(parsers),
    do: fn input ->
      case parsers do
        [] ->
          {:ok, [], input}

        [head | tail] ->
          with {:ok, term, rest} <- head.(input),
               {:ok, other_terms, rest} <- sequence(tail).(rest),
               do: {:ok, [term | other_terms], rest}
      end
    end

  @spec separated_list(element_parser :: parser(), separator_parser :: parser()) :: parser()
  def separated_list(element_parser, separator_parser),
    do:
      sequence([
        element_parser,
        repeat(sequence([separator_parser, element_parser]))
      ])
      |> map(fn [first_element, rest] ->
        other_elements = rest |> Enum.map(fn [_, element] -> element end)
        [first_element | other_elements]
      end)

  @spec surrounded_by(
          element_parser :: parser(),
          left_parser :: parser() | nil,
          right_parser :: parser() | nil
        ) :: parser()
  def surrounded_by(element_parser, left_parser \\ nil, right_parser \\ nil)
  def surrounded_by(_element_parser, nil, nil), do: {:error, "left and right expected"}

  def surrounded_by(element_parser, left_parser, nil),
    do: surrounded_by(element_parser, left_parser, left_parser)

  def surrounded_by(element_parser, left_parser, right_parser),
    do:
      [left_parser, element_parser, right_parser]
      |> Enum.reject(&(&1 == nil))
      |> sequence()
      |> map(fn [_, term, _] -> term end)

  def value(expected),
    do:
      expected
      |> String.to_charlist()
      |> Enum.map(&char/1)
      |> sequence()
      |> map(&to_string/1)

  def char(expected), do: satisfy(any(), fn char -> char == expected end)

  @spec any() :: parser()
  def any,
    do: fn
      "" -> {:error, "unexpected and of line"}
      <<char::utf8, rest::binary>> -> {:ok, char, rest}
    end
end
