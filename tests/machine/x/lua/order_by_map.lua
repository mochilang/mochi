data = {{["a"]=1, ["b"]=2}, {["a"]=1, ["b"]=1}, {["a"]=0, ["b"]=5}}
sorted = (function()
  local _res = {}
  for _, x in ipairs(data) do
    _res[#_res+1] = {__key = {["a"]=x.a, ["b"]=x.b}, __val = x}
  end
  local items = _res
  table.sort(items, function(a,b) return a.__key < b.__key end)
  local tmp = {}
  for _, it in ipairs(items) do tmp[#tmp+1] = it.__val end
  items = tmp
  _res = items
  return _res
end)()
print(table.concat(sorted, " "))
