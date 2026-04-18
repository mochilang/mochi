defmodule Main do
  def is_match(s, p), do: go(s, p, 0, 0, -1, 0)
  defp go(s, p, i, j, star, match) do
    cond do
      i < String.length(s) and j < String.length(p) and (String.at(p, j) == "?" or String.at(p, j) == String.at(s, i)) -> go(s, p, i + 1, j + 1, star, match)
      j < String.length(p) and String.at(p, j) == "*" -> go(s, p, i, j + 1, j, i)
      i >= String.length(s) -> trailing_stars?(p, j)
      star != -1 -> go(s, p, match + 1, star + 1, star, match + 1)
      true -> false
    end
  end
  defp trailing_stars?(p, j) do
    if j >= String.length(p) do
      true
    else
      if String.at(p, j) == "*" do
        trailing_stars?(p, j + 1)
      else
        false
      end
    end
  end
  def main do
    lines = IO.read(:eof) |> String.split("\n", trim: false) |> Enum.map(&String.replace(&1, "\r", ""))
    case lines do
      [] -> :ok
      [tstr | rest] when tstr != "" ->
        {t, _} = Integer.parse(String.trim(tstr))
        {out, _} = Enum.reduce(1..t, {[], rest}, fn _, {acc, xs} ->
          [nstr | xs] = xs
          {n, _} = Integer.parse(String.trim(nstr))
          {s, xs} = if n > 0, do: {hd(xs), tl(xs)}, else: {"", xs}
          [mstr | xs] = xs
          {m, _} = Integer.parse(String.trim(mstr))
          {p, xs} = if m > 0, do: {hd(xs), tl(xs)}, else: {"", xs}
          {[if(is_match(s, p), do: "true", else: "false") | acc], xs}
        end)
        IO.write(Enum.reverse(out) |> Enum.join("\n"))
      _ -> :ok
    end
  end
end
Main.main()
