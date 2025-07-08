nums = [2,7,11,15]

def two_sum(nums, target) do
  n = length(nums)
  Enum.reduce_while(0..(n-1), [-1, -1], fn i, _acc ->
    j = Enum.find(i+1..(n-1), fn j -> Enum.at(nums, i) + Enum.at(nums, j) == target end)
    if j do
      {:halt, [i, j]}
    else
      {:cont, [-1, -1]}
    end
  end)
end

[result_i, result_j] = two_sum(nums, 9)
IO.puts(result_i)
IO.puts(result_j)
