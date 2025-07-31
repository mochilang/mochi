function boom()
  print("boom")
  return true
end
print((((((1 < 2) and (2 < 3)) and (3 < 4))) and (1) or (0)))
print((((((1 < 2) and (2 > 3)) and boom())) and (1) or (0)))
print(((((((1 < 2) and (2 < 3)) and (3 > 4)) and boom())) and (1) or (0)))
