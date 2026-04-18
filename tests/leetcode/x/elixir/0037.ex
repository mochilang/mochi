defmodule Main do
  def valid(board, r, c, ch) do
    Enum.all?(0..8, fn i -> Enum.at(Enum.at(board, r), i) != ch and Enum.at(Enum.at(board, i), c) != ch end) and
      Enum.all?((div(r, 3) * 3)..(div(r, 3) * 3 + 2), fn i -> Enum.all?((div(c, 3) * 3)..(div(c, 3) * 3 + 2), fn j -> Enum.at(Enum.at(board, i), j) != ch end) end)
  end
  def solve(board) do
    case Enum.find_value(0..8, fn r -> Enum.find_value(0..8, fn c -> if Enum.at(Enum.at(board, r), c) == ".", do: {r, c}, else: nil end) end) do
      nil -> {:ok, board}
      {r, c} ->
        Enum.find_value(~w(1 2 3 4 5 6 7 8 9), fn ch ->
          if valid(board, r, c, ch) do
            row = List.replace_at(Enum.at(board, r), c, ch)
            board2 = List.replace_at(board, r, row)
            case solve(board2) do {:ok, ans} -> {:ok, ans}; _ -> nil end
          end
        end) || :fail
    end
  end
  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: true)
    if lines != [] do
      t = hd(lines) |> String.to_integer()
      {_, out} = Enum.reduce(1..t, {1, []}, fn _, {idx, out} ->
        board = Enum.map(idx..(idx + 8), fn i -> Enum.at(lines, i) |> String.graphemes() end)
        {:ok, solved} = solve(board)
        {idx + 9, out ++ Enum.map(solved, &Enum.join/1)}
      end)
      IO.write(Enum.join(out, "\n"))
    end
  end
end
Main.main()
