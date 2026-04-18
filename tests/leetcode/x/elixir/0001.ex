defmodule Main do
  def two_sum(nums, target) do
    do_outer(nums, target, 0)
  end

  defp do_outer(nums, target, i) when i >= length(nums), do: {0, 0}

  defp do_outer(nums, target, i) do
    case do_inner(nums, target, i, i + 1) do
      nil -> do_outer(nums, target, i + 1)
      ans -> ans
    end
  end

  defp do_inner(nums, _target, _i, j) when j >= length(nums), do: nil

  defp do_inner(nums, target, i, j) do
    if Enum.at(nums, i) + Enum.at(nums, j) == target do
      {i, j}
    else
      do_inner(nums, target, i, j + 1)
    end
  end

  def run do
    tokens =
      IO.read(:all)
      |> String.split(~r/\s+/, trim: true)

    if tokens != [] do
      [t | rest] = Enum.map(tokens, &String.to_integer/1)
      solve(rest, t, [])
      |> Enum.reverse()
      |> Enum.each(&IO.puts/1)
    end
  end

  defp solve(_values, 0, acc), do: acc

  defp solve([n, target | rest], t, acc) do
    {nums, tail} = Enum.split(rest, n)
    {a, b} = two_sum(nums, target)
    solve(tail, t - 1, ["#{a} #{b}" | acc])
  end
end

Main.run()
