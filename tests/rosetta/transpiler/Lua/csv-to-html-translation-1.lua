-- Generated by Mochi v0.10.42 on 2025-07-28 11:14 GMT+7
function input()
  return io.read('*l')
end
local _nil = {}

local _now_seed = 0
local _now_seeded = false
do
  local s = os.getenv("MOCHI_NOW_SEED")
  if s and s ~= "" then
    local v = tonumber(s)
    if v then
      _now_seed = v
      _now_seeded = true
    end
  end
end
local function _now()
if _now_seeded then
  -- keep the seed within safe integer range for Lua (53 bits)
  _now_seed = (_now_seed * 1664525 + 1013904223) % 9007199254740991
  return _now_seed % 1000000000
end
return os.time() * 1000000000 + math.floor(os.clock() * 1000000000)
end

local function _split(s, sep)
local t = {}
local pattern = string.format("([^%s]+)", sep)
string.gsub(s, pattern, function(c) t[#t+1] = c end)
return t
end
do
  collectgarbage()
  local _bench_start_mem = collectgarbage('count') * 1024
  local _bench_start = _now()
  c = ((((("Character,Speech\n" .. "The multitude,The messiah! Show us the messiah!\n") .. "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n") .. "The multitude,Who are you?\n") .. "Brians mother,I'm his mother; that's who!\n") .. "The multitude,Behold his mother! Behold his mother!")
  rows = {}
  for _, line in ipairs(_split(c, "\n")) do
    rows = (function(lst, item)
    local res = {table.unpack(lst)}
    table.insert(res, item)
    return res
  end)(rows, _split(line, ","))
end
print("<table>")
for _, row in ipairs(rows) do
  cells = ""
  for _, cell in ipairs(row) do
    cells = (((tostring(cells) .. "<td>") .. tostring(cell)) .. "</td>")
  end
  print((("    <tr>" .. tostring(cells)) .. "</tr>"))
end
print("</table>")
local _bench_end = _now()
collectgarbage()
local _bench_end_mem = collectgarbage('count') * 1024
local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
end;
