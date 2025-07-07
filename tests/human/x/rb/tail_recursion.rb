def sum_rec(n, acc)
  return acc if n == 0
  sum_rec(n - 1, acc + n)
end

puts sum_rec(10, 0)
