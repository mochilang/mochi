-- Computer Language Benchmarks Game binary_trees, hand-written Lua.
-- Trees are nested 2-element arrays (Lua tables with [1]=left, [2]=right);
-- a leaf is the empty table {}.

local function make_tree(depth)
  if depth == 0 then
    return {}
  end
  return { make_tree(depth - 1), make_tree(depth - 1) }
end

local function check_tree(t)
  if #t == 0 then
    return 1
  end
  return 1 + check_tree(t[1]) + check_tree(t[2])
end

local depth = {{ .N }}
local iters = 1
for _ = 1, depth do iters = iters * 2 end

local total = 0
local start = os.clock()
for _ = 1, iters do
  total = total + check_tree(make_tree(depth))
end
local duration_us = (os.clock() - start) * 1e6

io.write(string.format('{"duration_us": %d, "output": %s}\n', math.floor(duration_us), tostring(total)))
