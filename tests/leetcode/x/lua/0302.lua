local lines = {}
for line in io.lines() do
  local trimmed = line:match("^%s*(.-)%s*$")
  if trimmed ~= "" then
    table.insert(lines, trimmed)
  end
end
if #lines == 0 then
  return
end

local t = tonumber(lines[1])
local idx = 2
local blocks = {}
for _ = 1, t do
  local parts = {}
  for token in lines[idx]:gmatch("%S+") do table.insert(parts, token) end
  idx = idx + 1
  local r = tonumber(parts[1])
  local image = {}
  for i = 1, r do
    image[i] = lines[idx]
    idx = idx + 1
  end
  idx = idx + 1
  local top, bottom = r, -1
  local left, right = #image[1], -1
  for i = 1, #image do
    for j = 1, #image[i] do
      if image[i]:sub(j, j) == "1" then
        if i - 1 < top then top = i - 1 end
        if i - 1 > bottom then bottom = i - 1 end
        if j - 1 < left then left = j - 1 end
        if j - 1 > right then right = j - 1 end
      end
    end
  end
  table.insert(blocks, tostring((bottom - top + 1) * (right - left + 1)))
end
io.write(table.concat(blocks, "\n\n"))
