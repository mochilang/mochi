defmodule Main do
  def justify(words, max_width), do: do_justify(words, max_width, []) |> Enum.reverse()
  defp do_justify([], _max_width, acc), do: acc
  defp do_justify(words, max_width, acc) do
    {line_words, rest, total} = take_line(words, max_width, [], 0)
    gaps = length(line_words) - 1
    line = if rest == [] or gaps == 0 do
      s = Enum.join(line_words, " ")
      s <> String.duplicate(" ", max_width - String.length(s))
    else
      spaces = max_width - total
      base = div(spaces, gaps)
      extra = rem(spaces, gaps)
      build_full(line_words, base, extra, "")
    end
    do_justify(rest, max_width, [line | acc])
  end
  defp take_line([], _max_width, acc, total), do: {Enum.reverse(acc), [], total}
  defp take_line([w | rest] = words, max_width, acc, total) do
    if total + String.length(w) + length(acc) <= max_width, do: take_line(rest, max_width, [w | acc], total + String.length(w)), else: {Enum.reverse(acc), words, total}
  end
  defp build_full([w], _base, _extra, acc), do: acc <> w
  defp build_full([w | rest], base, extra, acc), do: build_full(rest, base, max(0, extra - 1), acc <> w <> String.duplicate(" ", base + if(extra > 0, do: 1, else: 0)))
  def main do
    lines = IO.read(:eof) |> String.split("\n", trim: true)
    case lines do
      [] -> :ok
      [tstr | rest] ->
        {t, _} = Integer.parse(tstr)
        {out, _} = Enum.reduce(1..t, {[], rest}, fn tc, {acc, xs} ->
          [nstr | xs1] = xs
          {n, _} = Integer.parse(nstr)
          {words, xs2} = Enum.split(xs1, n)
          [wstr | xs3] = xs2
          {width, _} = Integer.parse(wstr)
          ans = justify(words, width)
          block = [Integer.to_string(length(ans))] ++ Enum.map(ans, fn s -> "|" <> s <> "|" end) ++ if(tc < t, do: ["="], else: [])
          {acc ++ block, xs3}
        end)
        IO.write(Enum.join(out, "\n"))
    end
  end
end
Main.main()
