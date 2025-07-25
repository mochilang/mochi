-- Generated by Mochi v0.10.40 on 2025-07-25 21:10 GMT+7
function input()
  return io.read('*l')
end
local _nil = {}

local function _parseIntStr(str)
local n = tonumber(str, 10)
if n == nil then return 0 end
return math.floor(n)
end

local function slice(lst, s, e)
if s < 0 then s = #lst + s end
if e == nil then e = #lst end
local r = {}
for i = s + 1, e do
  r[#r+1] = lst[i]
end
return r
end
function join(xs, sep)
  local res = ""
  local i = 0
  while (i < (function(v)
  if type(v) == 'table' and v.items ~= nil then
    return #v.items
  elseif type(v) == 'table' and (v[1] == nil) then
      local c = 0
      for _ in pairs(v) do c = c + 1 end
      return c
    elseif type(v) == 'string' or type(v) == 'table' then
        return #v
      else
        return 0
      end
    end)(xs)) do
      if (i > 0) then
        res = (res .. sep)
      end
      res = (res .. xs[i + 1])
      i = (i + 1)
    end
    return res
  end;
  
  function parseIntStr(str)
    local i = 0
    local neg = false
    if (((function(v)
    if type(v) == 'table' and v.items ~= nil then
      return #v.items
    elseif type(v) == 'table' and (v[1] == nil) then
        local c = 0
        for _ in pairs(v) do c = c + 1 end
        return c
      elseif type(v) == 'string' or type(v) == 'table' then
          return #v
        else
          return 0
        end
      end)(str) > 0) and (string.sub(str, (0 + 1), 1) == "-")) then
        neg = true
        i = 1
      end
      local n = 0
      local digits = {__name = "GenType1", __order = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"}, ["0"] = 0, ["1"] = 1, ["2"] = 2, ["3"] = 3, ["4"] = 4, ["5"] = 5, ["6"] = 6, ["7"] = 7, ["8"] = 8, ["9"] = 9}
      while (i < (function(v)
      if type(v) == 'table' and v.items ~= nil then
        return #v.items
      elseif type(v) == 'table' and (v[1] == nil) then
          local c = 0
          for _ in pairs(v) do c = c + 1 end
          return c
        elseif type(v) == 'string' or type(v) == 'table' then
            return #v
          else
            return 0
          end
        end)(str)) do
          n = ((n * 10) + digits[string.sub(str, (i + 1), (i + 1))])
          i = (i + 1)
        end
        if neg then
          n = (-n)
        end
        return n
      end;
      
      rows = {{"A", "B", "C"}, {"1", "2", "3"}, {"4", "5", "6"}, {"7", "8", "9"}};
      
      rows[0 + 1] = (function(lst, item)
      local res = {table.unpack(lst)}
      table.insert(res, item)
      return res
    end)(rows[0 + 1], "SUM");
    
    i = 1;
    
    while (i < (function(v)
    if type(v) == 'table' and v.items ~= nil then
      return #v.items
    elseif type(v) == 'table' and (v[1] == nil) then
        local c = 0
        for _ in pairs(v) do c = c + 1 end
        return c
      elseif type(v) == 'string' or type(v) == 'table' then
          return #v
        else
          return 0
        end
      end)(rows)) do
        sum = 0
        for _, s in ipairs(rows[i + 1]) do
          sum = (sum + _parseIntStr(s))
        end
        rows[i + 1] = (function(lst, item)
        local res = {table.unpack(lst)}
        table.insert(res, item)
        return res
      end)(rows[i + 1], tostring(sum))
      i = (i + 1)
    end;
    
    for _, r in ipairs(rows) do
      print(join(r, ","))
    end;
