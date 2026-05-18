-- MEP-39 regex_redux Lua peer. LCG-generated DNA stream + 4-byte
-- rolling window matched against "agtt" (47) and "ttga" (248).
-- Output is the match count.

local N = {{ .N }}

local start = os.clock()
local seed = 42
local win = 0
local count = 0
for i = 0, N - 1 do
  seed = (seed * 3877 + 29573) % 139968
  local code = math.floor((seed * 4) / 139968)
  win = ((win * 4) % 256) + code
  if i >= 3 then
    if win == 47 or win == 248 then
      count = count + 1
    end
  end
end

local duration_us = (os.clock() - start) * 1e6
io.write(string.format('{"duration_us": %d, "output": %d}\n', math.floor(duration_us), count))
