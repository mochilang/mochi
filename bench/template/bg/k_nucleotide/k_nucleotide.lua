-- MEP-39 k_nucleotide Lua peer. LCG random generator + HOMO_SAPIENS
-- 4-entry cumprob lookup returning 2-bit codes 0..3; int-keyed table
-- holds 1-mer (keys 0..3) and 2-mer (keys 4..19) counts; output is
-- a rolling i64 hash over the 20 counts, integer-comparable across
-- all peers.

local N = {{ .N }}

local function lookup(prob)
  if prob < 0.3029549426680 then return 0
  elseif prob < 0.5009432431601 then return 1
  elseif prob < 0.6984905497992 then return 2
  else return 3 end
end

local start = os.clock()
local counts = {}
local seed = (42 * 3877 + 29573) % 139968
local prev = lookup(seed / 139968)
counts[prev] = 1
for _ = 1, N - 1 do
  seed = (seed * 3877 + 29573) % 139968
  local code = lookup(seed / 139968)
  counts[code] = (counts[code] or 0) + 1
  local key2 = 4 + prev * 4 + code
  counts[key2] = (counts[key2] or 0) + 1
  prev = code
end

local h = 0
for k = 0, 19 do
  h = (h * 1009 + (counts[k] or 0)) % 2147483647
end

local duration_us = (os.clock() - start) * 1e6
io.write(string.format('{"duration_us": %d, "output": %d}\n', math.floor(duration_us), h))
