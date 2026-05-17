-- MEP-39 fannkuch_redux Lua peer. Mirrors fannkuch_redux.mochi shape.
-- Note: Lua tables are 1-indexed, so perm[1..7] is the 7-element
-- permutation. The output integer matches the Mochi peer because the
-- algorithm is identical; the index base is internal.

local function count_flips(perm)
  local flips = 0
  local head = perm[1]
  while head ~= 1 do
    local lo, hi = 1, head
    while lo < hi do
      perm[lo], perm[hi] = perm[hi], perm[lo]
      lo = lo + 1
      hi = hi - 1
    end
    head = perm[1]
    flips = flips + 1
  end
  return flips
end

local trials = {{ .N }}
local total = 0
local perm = {0, 0, 0, 0, 0, 0, 0}

local start = os.clock()
for k = 0, trials - 1 do
  for i = 0, 6 do
    perm[i + 1] = ((i + k) % 7) + 1
  end
  total = total + count_flips(perm)
end
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(total)))
