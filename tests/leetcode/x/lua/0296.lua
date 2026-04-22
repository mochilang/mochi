local data = {}
for token in io.read("*a"):gmatch("%S+") do
  table.insert(data, tonumber(token))
end
if #data == 0 then
  return
end

local idx = 1
local t = data[idx]
idx = idx + 1
local blocks = {}
for _ = 1, t do
  local r = data[idx]
  local c = data[idx + 1]
  idx = idx + 2
  local grid = {}
  local rows = {}
  local cols = {}
  for i = 1, r do
    grid[i] = {}
    for j = 1, c do
      grid[i][j] = data[idx]
      idx = idx + 1
      if grid[i][j] == 1 then
        table.insert(rows, i - 1)
      end
    end
  end
  for j = 1, c do
    for i = 1, r do
      if grid[i][j] == 1 then
        table.insert(cols, j - 1)
      end
    end
  end
  local mr = rows[math.floor(#rows / 2) + 1]
  local mc = cols[math.floor(#cols / 2) + 1]
  local ans = 0
  for _, x in ipairs(rows) do ans = ans + math.abs(x - mr) end
  for _, x in ipairs(cols) do ans = ans + math.abs(x - mc) end
  table.insert(blocks, tostring(ans))
end
io.write(table.concat(blocks, "\n\n"))
