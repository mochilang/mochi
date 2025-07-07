local function sum_rec(n, acc)
  if n == 0 then
    return acc
  end
  return sum_rec(n - 1, acc + n)
end
print(sum_rec(10, 0))
