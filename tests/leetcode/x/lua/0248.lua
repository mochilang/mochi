local pairs = {
  {"0", "0"}, {"1", "1"}, {"6", "9"}, {"8", "8"}, {"9", "6"}
}

local function build(n, m)
  if n == 0 then return {""} end
  if n == 1 then return {"0", "1", "8"} end
  local mids = build(n - 2, m)
  local res = {}
  for _, mid in ipairs(mids) do
    for _, p in ipairs(pairs) do
      if not (n == m and p[1] == "0") then
        res[#res + 1] = p[1] .. mid .. p[2]
      end
    end
  end
  return res
end

local function count_range(low, high)
  local ans = 0
  for len = #low, #high do
    for _, s in ipairs(build(len, len)) do
      if not (len == #low and s < low) and not (len == #high and s > high) then
        ans = ans + 1
      end
    end
  end
  return ans
end

local lines = {}
for line in io.lines() do
  lines[#lines + 1] = line
end
if #lines > 0 then
  local t = tonumber(lines[1])
  local idx = 2
  local out = {}
  for _ = 1, t do
    out[#out + 1] = tostring(count_range(lines[idx], lines[idx + 1]))
    idx = idx + 2
  end
  io.write(table.concat(out, "\n"))
end
