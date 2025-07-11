function boom(a, b)
  print("boom")
  return true
end

print((false and boom(1, 2)))
print((true or boom(1, 2)))
