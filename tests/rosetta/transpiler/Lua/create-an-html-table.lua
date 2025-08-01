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
do
  collectgarbage()
  local _bench_start_mem = collectgarbage('count') * 1024
  local _bench_start = _now()
  function main()
    local rows = {}
    for i = 0, 4 - 1 do
      rows = (function(lst, item)
      local res = {table.unpack(lst)}
      table.insert(res, item)
      return res
    end)(rows, {(i * 3), ((i * 3) + 1), ((i * 3) + 2)})
  end
  print("<table>")
  print("    <tr><th></th><th>X</th><th>Y</th><th>Z</th></tr>")
  local idx = 0
  for _, row in ipairs(rows) do
    print((((((((("    <tr><td>" .. tostring(idx)) .. "</td><td>") .. tostring(row[0 + 1])) .. "</td><td>") .. tostring(row[1 + 1])) .. "</td><td>") .. tostring(row[2 + 1])) .. "</td></tr>"))
    idx = (idx + 1)
  end
  print("</table>")
end
main()
local _bench_end = _now()
collectgarbage()
local _bench_end_mem = collectgarbage('count') * 1024
local _bench_duration_us = math.floor((_bench_end - _bench_start) / 1000)
local _bench_mem = math.floor(math.max(0, _bench_end_mem - _bench_start_mem))
print('{\n  "duration_us": ' .. _bench_duration_us .. ',\n  "memory_bytes": ' .. _bench_mem .. ',\n  "name": "main"\n}')
end;
