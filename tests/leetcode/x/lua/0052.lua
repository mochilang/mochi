local function solve(n)
  local cols, d1, d2, board, res = {}, {}, {}, {}, {}
  for i = 1, n do cols[i] = false; board[i] = string.rep('.', n) end
  for i = 1, 2 * n do d1[i] = false; d2[i] = false end
  local function dfs(r)
    if r > n then local sol = {}; for i = 1, n do sol[i] = board[i] end; table.insert(res, sol); return end
    for c = 1, n do
      local a, b = r + c - 1, r - c + n
      if not cols[c] and not d1[a] and not d2[b] then
        cols[c], d1[a], d2[b] = true, true, true
        board[r] = string.rep('.', c - 1) .. 'Q' .. string.rep('.', n - c)
        dfs(r + 1)
        board[r] = string.rep('.', n)
        cols[c], d1[a], d2[b] = false, false, false
      end
    end
  end
  dfs(1)
  return res
end
local lines = {}
for line in io.lines() do local cleaned = line:gsub("\r", ""); table.insert(lines, cleaned) end
if #lines == 0 or lines[1] == '' then return end
local idx = 1; local t = tonumber(lines[idx]); idx = idx + 1; local out = {}
for tc = 1, t do local n = tonumber(lines[idx]); idx = idx + 1; local sols = solve(n); table.insert(out, tostring(#sols)) end
io.write(table.concat(out, '\n'))
