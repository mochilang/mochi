local function matchAt(s, p, i, j)
  if j > #p then return i > #s end
  local first = i <= #s and (p:sub(j, j) == "." or s:sub(i, i) == p:sub(j, j))
  if j + 1 <= #p and p:sub(j + 1, j + 1) == "*" then
    return matchAt(s, p, i, j + 2) or (first and matchAt(s, p, i + 1, j))
  end
  return first and matchAt(s, p, i + 1, j + 1)
end

local t = tonumber(io.read("*l"))
if not t then os.exit() end
local out = {}
for _ = 1, t do
  local s = io.read("*l") or ""
  local p = io.read("*l") or ""
  out[#out + 1] = matchAt(s, p, 1, 1) and "true" or "false"
end
io.write(table.concat(out, "\n"))
