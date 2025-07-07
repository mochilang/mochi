local data = {
  {a = 1, b = 2},
  {a = 1, b = 1},
  {a = 0, b = 5},
}

table.sort(data, function(x, y)
  if x.a == y.a then
    return x.b < y.b
  else
    return x.a < y.a
  end
end)

for i, x in ipairs(data) do
  io.write("map[a:"..x.a.." b:"..x.b.."]")
  if i < #data then io.write(" ") end
end
io.write("\n")
