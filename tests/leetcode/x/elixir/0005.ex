defmodule Main do
  def expand(s, left, right) do
    if left >= 0 and right < byte_size(s) and :binary.at(s, left) == :binary.at(s, right) do
      expand(s, left - 1, right + 1)
    else
      {left + 1, right - left - 1}
    end
  end

  def longest_palindrome(s) do
    n = byte_size(s)
    {best_start, best_len} =
      Enum.reduce(0..max(n - 1, -1), {0, if(n > 0, do: 1, else: 0)}, fn i, {best_start, best_len} ->
        {start1, len1} = expand(s, i, i)
        {best_start, best_len} = if len1 > best_len, do: {start1, len1}, else: {best_start, best_len}
        {start2, len2} = expand(s, i, i + 1)
        if len2 > best_len, do: {start2, len2}, else: {best_start, best_len}
      end)

    binary_part(s, best_start, best_len)
  end

  def main do
    lines = IO.read(:stdio, :eof) |> String.split(~r/\r?\n/, trim: false)
    if lines != [] do
      t = hd(lines) |> String.trim() |> String.to_integer()
      ss = Enum.take(tl(lines) ++ List.duplicate("", t), t)
      IO.write(Enum.map_join(ss, "\n", &longest_palindrome/1))
    end
  end
end

Main.main()
