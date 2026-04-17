defmodule Main do
  def read_case(lines, idx, k, vals) do
    if k == 0 do
      {idx, vals}
    else
      n = Enum.at(lines, idx) |> String.to_integer()
      vals = Enum.reduce(0..(n - 1), vals, fn j, acc -> if n == 0, do: acc, else: acc ++ [Enum.at(lines, idx + 1 + j) |> String.to_integer()] end)
      read_case(lines, idx + 1 + n, k - 1, vals)
    end
  end
  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/?
/, trim: true)
    if lines != [] do
      t = hd(lines) |> String.to_integer()
      {_, out} = Enum.reduce(1..t, {1, []}, fn _, {idx, out} ->
        k = Enum.at(lines, idx) |> String.to_integer()
        {next_idx, vals} = read_case(lines, idx + 1, k, [])
        vals = Enum.sort(vals)
        {next_idx, out ++ ["[" <> Enum.join(Enum.map(vals, &Integer.to_string/1), ",") <> "]"]}
      end)
      IO.write(Enum.join(out, "
"))
    end
  end
end
Main.main()
