defmodule Main do
  def calculate(expr) do
    {result, number, sign, stack} =
      expr
      |> String.to_charlist()
      |> Enum.reduce({0, 0, 1, []}, fn ch, {result, number, sign, stack} ->
        cond do
          ch >= ?0 and ch <= ?9 ->
            {result, number * 10 + ch - ?0, sign, stack}

          ch == ?+ or ch == ?- ->
            {result + sign * number, 0, if(ch == ?+, do: 1, else: -1), stack}

          ch == ?( ->
            {0, 0, 1, [sign, result | stack]}

          ch == ?) ->
            [prev_sign, prev_result | rest] = stack
            {prev_result + prev_sign * (result + sign * number), 0, 1, rest}

          true ->
            {result, number, sign, stack}
        end
      end)

    result + sign * number
  end

  def main do
    lines = IO.read(:stdio, :all) |> String.split(~r/\r?\n/, trim: false)
    if lines != [] and String.trim(hd(lines)) != "" do
      t = hd(lines) |> String.trim() |> String.to_integer()
      lines
      |> tl()
      |> Enum.take(t)
      |> Enum.map(&(calculate(&1) |> Integer.to_string()))
      |> Enum.join("\n")
      |> IO.write()
    end
  end
end

Main.main()
