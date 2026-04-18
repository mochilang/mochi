defmodule Main do
  def valid_number(s) do
    chars = String.to_charlist(s)

    case Enum.reduce_while(Enum.with_index(chars), {false, false, false, true}, fn {ch, i}, {seen_digit, seen_dot, seen_exp, digit_after_exp} ->
           cond do
             ch >= ?0 and ch <= ?9 ->
               {:cont, {true, seen_dot, seen_exp, if(seen_exp, do: true, else: digit_after_exp)}}

             ch == ?+ or ch == ?- ->
               if i != 0 and Enum.at(chars, i - 1) != ?e and Enum.at(chars, i - 1) != ?E do
                 {:halt, :bad}
               else
                 {:cont, {seen_digit, seen_dot, seen_exp, digit_after_exp}}
               end

             ch == ?. ->
               if seen_dot or seen_exp do
                 {:halt, :bad}
               else
                 {:cont, {seen_digit, true, seen_exp, digit_after_exp}}
               end

             ch == ?e or ch == ?E ->
               if seen_exp or not seen_digit do
                 {:halt, :bad}
               else
                 {:cont, {seen_digit, seen_dot, true, false}}
               end

             true ->
               {:halt, :bad}
           end
         end) do
      {seen_digit, _, _, digit_after_exp} -> seen_digit and digit_after_exp
      _ -> false
    end
  end

  def main do
    lines = IO.read(:eof) |> String.split("\n", trim: true)

    case lines do
      [] ->
        :ok

      [tstr | rest] ->
        {t, _} = Integer.parse(String.trim(tstr))
        out = for i <- 0..(t - 1), do: if(valid_number(Enum.at(rest, i)), do: "true", else: "false")
        IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
