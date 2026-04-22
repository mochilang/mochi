local function solve(num, target)
  local ans = {}
  local function dfs(i, expr, value, last)
    if i > #num then
      if value == target then ans[#ans + 1] = expr end
      return
    end
    for j = i, #num do
      if j > i and num:sub(i, i) == "0" then break end
      local s = num:sub(i, j)
      local n = tonumber(s)
      if i == 1 then
        dfs(j + 1, s, n, n)
      else
        dfs(j + 1, expr .. "+" .. s, value + n, n)
        dfs(j + 1, expr .. "-" .. s, value - n, -n)
        dfs(j + 1, expr .. "*" .. s, value - last + last * n, last * n)
      end
    end
  end
  dfs(1, "", 0, 0)
  table.sort(ans)
  return ans
end

local lines = {}
for line in io.lines() do lines[#lines + 1] = line end
if #lines > 0 then
  local t = tonumber(lines[1])
  local blocks = {}
  local idx = 2
  for _ = 1, t do
    local num = lines[idx]
    local target = tonumber(lines[idx + 1])
    idx = idx + 2
    local ans = solve(num, target)
    local part = { tostring(#ans) }
    for _, s in ipairs(ans) do part[#part + 1] = s end
    blocks[#blocks + 1] = table.concat(part, "\n")
  end
  io.write(table.concat(blocks, "\n\n"))
end
