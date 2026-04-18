defmodule Main do
  def first_missing_positive(nums) do
    nums = List.to_tuple(nums)
    n = tuple_size(nums)
    nums = place(nums, n, 0)
    scan(nums, n, 0)
  end

  defp place(nums, n, i) when i >= n, do: nums
  defp place(nums, n, i) do
    v = elem(nums, i)
    cond do
      v >= 1 and v <= n and elem(nums, v - 1) != v ->
        target = elem(nums, v - 1)
        nums = nums |> put_elem(i, target) |> put_elem(v - 1, v)
        place(nums, n, i)
      true ->
        place(nums, n, i + 1)
    end
  end

  defp scan(_nums, n, i) when i >= n, do: n + 1
  defp scan(nums, n, i) do
    if elem(nums, i) != i + 1, do: i + 1, else: scan(nums, n, i + 1)
  end

  def main do
    lines = IO.read(:eof) |> String.split("\n", trim: true)
    case lines do
      [] -> :ok
      [tstr | rest] ->
        {t, _} = Integer.parse(String.trim(tstr))
        {out, _} = Enum.reduce(1..t, {[], rest}, fn _, {acc, xs} ->
          [nstr | xs] = xs
          {n, _} = Integer.parse(String.trim(nstr))
          {vals, xs} = Enum.split(xs, n)
          nums = Enum.map(vals, fn s -> elem(Integer.parse(String.trim(s)), 0) end)
          {[Integer.to_string(first_missing_positive(nums)) | acc], xs}
        end)
        IO.write(Enum.reverse(out) |> Enum.join("\n"))
    end
  end
end

Main.main()
