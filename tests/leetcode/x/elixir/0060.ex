defmodule Main do
  def get_permutation(n, k_input) do
    digits = Enum.map(1..n, &Integer.to_string/1)
    fact = Enum.reduce(1..n, [1], fn i, acc -> acc ++ [List.last(acc) * i] end)
    build(digits, k_input - 1, n, fact, "")
  end

  defp build(_digits, _k, 0, _fact, out), do: out
  defp build(digits, k, rem, fact, out) do
    block = Enum.at(fact, rem - 1)
    idx = div(k, block)
    k2 = rem(k, block)
    pick = Enum.at(digits, idx)
    rest = List.delete_at(digits, idx)
    build(rest, k2, rem - 1, fact, out <> pick)
  end

  def main do
    lines = IO.read(:eof) |> String.split("\n", trim: true)
    case lines do
      [] -> :ok
      [tstr | rest] ->
        {t, _} = Integer.parse(String.trim(tstr))
        {out, _} = Enum.reduce(1..t, {[], rest}, fn _, {acc, xs} ->
          [nstr, kstr | tail] = xs
          {n, _} = Integer.parse(String.trim(nstr))
          {k, _} = Integer.parse(String.trim(kstr))
          {acc ++ [get_permutation(n, k)], tail}
        end)
        IO.write(Enum.join(out, "\n"))
    end
  end
end
Main.main()
