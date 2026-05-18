-- MEP-39 fasta Lua peer. LCG random generator + HOMO_SAPIENS
-- 4-entry cumprob lookup + rolling i64 hash. Output is the final
-- hash, integer-comparable across all peers.

local N = {{ .N }}

local function lookup(prob)
  if prob < 0.3029549426680 then return 97
  elseif prob < 0.5009432431601 then return 99
  elseif prob < 0.6984905497992 then return 103
  else return 116 end
end

local start = os.clock()
local seed = 42
local h = 0
for _ = 1, N do
  seed = (seed * 3877 + 29573) % 139968
  local prob = seed / 139968
  local b = lookup(prob)
  h = (h * 1009 + b) % 2147483647
end

local duration_us = (os.clock() - start) * 1e6
io.write(string.format('{"duration_us": %d, "output": %d}\n', math.floor(duration_us), h))
