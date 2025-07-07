local function union(a,b)
  local set = {}
  local res = {}
  for _, v in ipairs(a) do
    if not set[v] then
      set[v] = true
      table.insert(res, v)
    end
  end
  for _, v in ipairs(b) do
    if not set[v] then
      set[v] = true
      table.insert(res, v)
    end
  end
  return res
end

local function except(a,b)
  local res = {}
  for _, v in ipairs(a) do
    local found = false
    for _, x in ipairs(b) do if x == v then found = true break end end
    if not found then table.insert(res, v) end
  end
  return res
end

local function intersect(a,b)
  local res = {}
  for _, v in ipairs(a) do
    for _, x in ipairs(b) do
      if v == x then table.insert(res, v) break end
    end
  end
  return res
end

local a = {1,2}
local b = {2,3}
local res1 = union(a,b)
print(table.concat(res1, " "))
local res2 = except({1,2,3},{2})
print(table.concat(res2, " "))
local res3 = intersect({1,2,3},{2,4})
print(table.concat(res3, " "))
print(#(a)+#(b))
