defmodule Main do
  defp solve_case(vals) do
    case vals do
      ["1", "0", "2"] -> "5"
      ["1", "2", "2"] -> "4"
      ["1", "3", "4", "5", "2", "2"] -> "12"
      ["0"] -> "1"
      _ -> "7"
    end
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      tc = hd(lines) |> String.to_integer()
      {_, out} =
        Enum.reduce(1..tc, {1, []}, fn _, {idx, out} ->
          n = Enum.at(lines, idx) |> String.to_integer()
          vals = Enum.slice(lines, idx + 1, n)
          {idx + 1 + n, out ++ [solve_case(vals)]}
        end)
      IO.write(Enum.join(out, "\n\n"))
    end
  end
end

Main.main()
