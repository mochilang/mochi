defmodule Main do
  def convert(s, num_rows) do
    if num_rows <= 1 or num_rows >= byte_size(s) do
      s
    else
      cycle = 2 * num_rows - 2

      0..(num_rows - 1)
      |> Enum.map(fn row ->
        Enum.reduce(Stream.iterate(row, &(&1 + cycle)) |> Enum.take_while(&(&1 < byte_size(s))), "", fn i, acc ->
          acc = acc <> binary_part(s, i, 1)
          diag = i + cycle - 2 * row
          if row > 0 and row < num_rows - 1 and diag < byte_size(s) do
            acc <> binary_part(s, diag, 1)
          else
            acc
          end
        end)
      end)
      |> Enum.join("")
    end
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: false)
    if lines != [] do
      t = hd(lines) |> String.trim() |> String.to_integer()
      {_, _, out} =
        Enum.reduce(1..t, {1, lines, []}, fn _, {idx, lines, out} ->
          s = Enum.at(lines, idx, "")
          num_rows = Enum.at(lines, idx + 1, "1") |> String.trim() |> String.to_integer()
          {idx + 2, lines, out ++ [convert(s, num_rows)]}
        end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end

Main.main()
