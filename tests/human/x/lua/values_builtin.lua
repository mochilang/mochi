local m = {a=1,b=2,c=3}
local vals = {}
for _,v in pairs(m) do table.insert(vals,v) end
print(table.concat(vals, " "))
