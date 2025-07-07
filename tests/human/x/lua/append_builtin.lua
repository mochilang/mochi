local a = {1, 2}
table.insert(a, 3)
for i, v in ipairs(a) do
  io.write(v)
  if i < #a then io.write(" ") end
end
io.write("\n")
