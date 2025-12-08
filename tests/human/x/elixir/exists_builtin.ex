data = [1,2]
flag = Enum.any?(data, fn x -> x == 1 end)
IO.inspect(flag)
