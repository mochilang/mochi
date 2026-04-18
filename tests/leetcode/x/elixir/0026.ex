defmodule Solution do
  def remove_duplicates(nums) do
    nums
    |> Enum.chunk_by(& &1)
    |> Enum.map(&hd/1)
  end

  def solve() do
    input = IO.read(:all)
    tokens = String.split(input)
    if tokens != [] do
      [t_str | rem] = tokens
      t = String.to_integer(t_str)
      do_solve(t, rem)
    end
  end

  def do_solve(0, _), do: :ok
  def do_solve(t, [n_str | rem]) do
    n = String.to_integer(n_str)
    {nums_str, rem} = Enum.split(rem, n)
    nums = Enum.map(nums_str, &String.to_integer/1)
    ans = remove_duplicates(nums)
    IO.puts Enum.join(ans, " ")
    do_solve(t - 1, rem)
  end
end

Solution.solve()
