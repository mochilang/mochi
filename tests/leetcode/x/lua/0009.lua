local data = io.read("*a")
if data == nil or data == "" then os.exit() end
local tokens = {}
for token in string.gmatch(data, "%S+") do
  table.insert(tokens, tonumber(token))
end

local function is_palindrome(x)
  if x < 0 then return false end
  local original = x
  local rev = 0
  while x > 0 do
    rev = rev * 10 + (x % 10)
    x = math.floor(x / 10)
  end
  return rev == original
end

local t = tokens[1]
for i = 1, t do
  io.write(is_palindrome(tokens[i + 1]) and "true" or "false")
  if i < t then io.write("\n") end
end
