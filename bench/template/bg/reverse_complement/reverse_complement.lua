-- MEP-39 reverse_complement Lua peer. Fill an N-byte buffer with the
-- ACGT cycle, reverse-complement into a second buffer (A<->T, C<->G),
-- and sum the output as int64 so the cross-lang harness can
-- integer-compare. When N is a multiple of 4 the result is
-- (N/4) * 287.

local N = {{ .N }}

local function complement(c)
  if c == 65 then return 84
  elseif c == 84 then return 65
  elseif c == 67 then return 71
  elseif c == 71 then return 67
  else return c end
end

local bases = {65, 67, 71, 84}
local in_buf = {}
local out_buf = {}

local start = os.clock()
for i = 0, N - 1 do
  in_buf[i + 1] = bases[(i % 4) + 1]
end
for i = 0, N - 1 do
  out_buf[N - i] = complement(in_buf[i + 1])
end

local total = 0
for i = 1, N do
  total = total + out_buf[i]
end

local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %d}\n', math.floor(duration_us), total))
