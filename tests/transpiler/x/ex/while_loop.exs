# Generated by Mochi transpiler on 2025-07-21 20:06 +0700
i = 0
while_fun = fn while_fun, i ->
  if i < 3 do
    IO.puts(i)
    i = i + 1
    while_fun.(while_fun, i)
  else
    i
  end
end
i = while_fun.(while_fun, i)
