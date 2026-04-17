local data = io.read("*a")
if data == nil or data == "" then os.exit() end
local tokens = {}
for token in string.gmatch(data, "%S+") do table.insert(tokens, token) end

local function is_valid(s)
  local stack = {}
  for i = 1, #s do
    local ch = string.sub(s, i, i)
    if ch == "(" or ch == "[" or ch == "{" then
      table.insert(stack, ch)
    else
      if #stack == 0 then return false end
      local open = stack[#stack]
      stack[#stack] = nil
      if (ch == ")" and open ~= "(") or (ch == "]" and open ~= "[") or (ch == "}" and open ~= "{") then
        return false
      end
    end
  end
  return #stack == 0
end

local t = tonumber(tokens[1])
for i = 1, t do
  io.write(is_valid(tokens[i + 1]) and "true" or "false")
  if i < t then io.write("\n") end
end
