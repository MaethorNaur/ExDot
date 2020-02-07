defmodule ExDot.Parser.Combinator do
  @type success :: {:ok, term(), binary()}
  @type error :: {:error, term()}
  @type result :: success() | error()
  @type parser :: (binary() -> result())

  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [debug: Keyword.get(opts, :debug, false)] do
      Module.put_attribute(__MODULE__, :debug, debug)

      defmacrop lhs ~> rhs do
        sequence = _to_list_sequence(lhs, rhs)
        quote do: sequence(unquote(sequence))
      end

      defmacrop lhs ~>> rhs do
        lhs = _expand_macro(lhs)
        quote do: map(unquote(lhs), unquote(rhs))
      end

      defmacrop maybe(rhs), do: quote(do: optional(unquote(_expand_macro(rhs))))

      defmacrop lhs | rhs do
        list = _to_list_choice(rhs, lhs) |> Enum.reverse()
        quote do: choice(unquote(list))
      end

      defp _to_list_sequence(lhs, rhs, acc \\ [])

      defp _to_list_sequence({:~>, _, [lhs, rhs]}, term, acc),
        do: _to_list_sequence(lhs, rhs, [_expand_macro(term) | acc])

      defp _to_list_sequence(lhs, rhs, acc),
        do: [_expand_macro(lhs), _expand_macro(rhs) | acc]

      defp _to_list_choice(lhs, rhs, acc \\ [])

      defp _to_list_choice(term, {:|, _, [lhs, rhs]}, acc),
        do: _to_list_choice(lhs, rhs, [_expand_macro(term) | acc])

      defp _to_list_choice(lhs, rhs, acc),
        do: [_expand_macro(lhs), _expand_macro(rhs) | acc]

      defp _expand_macro({atom, _, _} = expand) when atom in [:|, :~>],
        do: Macro.expand(expand, __ENV__)

      defp _expand_macro(input) when is_binary(input), do: quote(do: value(unquote(input)))
      defp _expand_macro(input) when is_integer(input), do: quote(do: char(unquote(input)))
      defp _expand_macro([input]) when is_integer(input), do: quote(do: char(unquote(input)))
      defp _expand_macro(input), do: input

      defp named(parser, name),
        do: fn input ->
          if @debug, do: IO.puts("#{name} running for: #{input}")

          case parser.(input) do
            {:error, _reason} ->
              {:error, name}

            {:ok, term, rest} ->
              if @debug, do: IO.puts("#{name} result: #{inspect(term)}, #{rest}")
              {:ok, term, rest}
          end
        end

      @spec optional(parser :: ExDot.Parser.Combinator.parser()) ::
              ExDot.Parser.Combinator.parser()
      defp optional(parser),
        do: fn input ->
          with {:error, _reason} <- parser.(input) do
            {:ok, nil, input}
          end
        end

      @spec lazy(combinator :: (() -> ExDot.Parser.Combinator.parser())) ::
              ExDot.Parser.Combinator.parser()
      defp lazy(combinator),
        do: fn input ->
          parser = combinator.()
          parser.(input)
        end

      @spec satisfy(
              parser :: ExDot.Parser.Combinator.parser(),
              predicate :: (term() -> boolean())
            ) :: ExDot.Parser.Combinator.parser()
      defp satisfy(parser, predicate),
        do: fn input ->
          with {:ok, term, rest} <- parser.(input) do
            if predicate.(term), do: {:ok, term, rest}, else: {:error, "term unsatisfy"}
          end
        end

      @spec repeat(
              parser :: ExDot.Parser.Combinator.parser(),
              min :: pos_integer(),
              max :: pos_integer()
            ) ::
              ExDot.Parser.Combinator.parser()
      defp repeat(parser, min \\ 0, max \\ 0) when min >= 0 and max >= 0,
        do: _repeat(parser, min, max, 0)

      defp repeat_until(parser, predicate, acc \\ []),
        do: fn input ->
          case parser.(input) do
            {:error, _reason} ->
              {:ok, [], input}

            {:ok, first_term, rest} ->
              acc = acc++[first_term ]  

              if predicate.(acc) do
                {:ok, other_terms, rest} = repeat_until(parser, predicate, acc).(rest)
                {:ok, [first_term | other_terms], rest}
              else
                {:ok, acc, rest}
              end
          end
        end

      @spec _repeat(
              parser :: ExDot.Parser.Combinator.parser(),
              min :: pos_integer,
              max :: pos_integer,
              counter :: pos_integer
            ) ::
              ExDot.Parser.Combinator.parser()
      defp _repeat(parser, min, max, counter),
        do: fn input ->
          case parser.(input) do
            {:error, reason} when min > 0 and min > counter ->
              {:error, "#{reason}{ #{min},#{max} }"}

            {:error, _reason} ->
              {:ok, [], input}

            {:ok, first_term, rest} when max > 0 and counter == max - 1 ->
              {:ok, [first_term], rest}

            {:ok, first_term, rest} ->
              {:ok, other_terms, rest} = _repeat(parser, min, max, counter + 1).(rest)
              {:ok, [first_term | other_terms], rest}
          end
        end

      @spec choice(parsers :: [ExDot.Parser.Combinator.parser()], acc :: [String.t()]) ::
              ExDot.Parser.Combinator.result()
      defp choice(parsers, acc \\ []),
        do: fn input ->
          case parsers do
            [] ->
              {:error, Enum.join(acc, " | ")}

            [head | tail] ->
              with {:error, reason} <- head.(input), do: choice(tail, [reason | acc]).(input)
          end
        end

      @spec map(parser :: ExDot.Parser.Combinator.parser(), mapper :: (term() -> term())) ::
              ExDot.Parser.Combinator.parser()
      defp map(parser, mapper),
        do: fn input ->
          with {:ok, term, rest} <- parser.(input), do: {:ok, mapper.(term), rest}
        end

      @spec sequence(parsers :: [ExDot.Parser.Combinator.parser()]) ::
              ExDot.Parser.Combinator.parser()
      defp sequence(parsers),
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

      @spec separated_list(
              element_parser :: ExDot.Parser.Combinator.parser(),
              separator_parser :: ExDot.Parser.Combinator.parser()
            ) :: ExDot.Parser.Combinator.parser()
      defp separated_list(element_parser, separator_parser),
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
              element_parser :: ExDot.Parser.Combinator.parser(),
              left_parser :: ExDot.Parser.Combinator.parser() | nil,
              right_parser :: ExDot.Parser.Combinator.parser() | nil
            ) :: ExDot.Parser.Combinator.parser()
      defp surrounded_by(element_parser, left_parser \\ nil, right_parser \\ nil)
      defp surrounded_by(_element_parser, nil, nil), do: {:error, "left and right expected"}

      defp surrounded_by(element_parser, left_parser, nil),
        do: surrounded_by(element_parser, left_parser, left_parser)

      defp surrounded_by(element_parser, left_parser, right_parser),
        do:
          [left_parser, element_parser, right_parser]
          |> Enum.reject(&(&1 == nil))
          |> sequence()
          |> map(fn [_, term, _] -> term end)

      defp value(expected),
        do:
          expected
          |> String.to_charlist()
          |> Enum.map(&char/1)
          |> sequence()
          |> map(&to_string/1)
          |> named(to_string(expected))

      defp char(expected),
        do:
          satisfy(any(), fn char -> char == expected end)
          |> named(to_string([expected]))

      @spec any() :: ExDot.Parser.Combinator.parser()
      defp any,
        do: fn
          "" -> {:error, "unexpected and of line"}
          <<char::utf8, rest::binary>> -> {:ok, char, rest}
        end
    end
  end
end
