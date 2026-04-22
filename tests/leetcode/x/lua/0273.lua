local less20 = {"", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
  "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"}
local tens = {"", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"}
local thousands = {"", "Thousand", "Million", "Billion"}

local function helper(n)
  if n == 0 then return "" end
  if n < 20 then return less20[n + 1] end
  if n < 100 then
    return tens[math.floor(n / 10) + 1] .. (n % 10 == 0 and "" or " " .. helper(n % 10))
  end
  return less20[math.floor(n / 100) + 1] .. " Hundred" .. (n % 100 == 0 and "" or " " .. helper(n % 100))
end

local function solve(num)
  if num == 0 then return "Zero" end
  local parts, idx = {}, 1
  while num > 0 do
    local chunk = num % 1000
    if chunk ~= 0 then
      local words = helper(chunk)
      if thousands[idx] ~= "" then words = words .. " " .. thousands[idx] end
      table.insert(parts, 1, words)
    end
    num = math.floor(num / 1000)
    idx = idx + 1
  end
  return table.concat(parts, " ")
end

local lines = {}
for line in io.lines() do lines[#lines + 1] = line end
if #lines > 0 then
  local t = tonumber(lines[1])
  local out = {}
  for i = 1, t do out[#out + 1] = solve(tonumber(lines[i + 1])) end
  io.write(table.concat(out, "\n"))
end
