local function calculate(expr)
  local result, number, sign = 0, 0, 1
  local stack = {}
  for i = 1, #expr do
    local ch = expr:sub(i, i)
    if ch >= "0" and ch <= "9" then
      number = number * 10 + tonumber(ch)
    elseif ch == "+" or ch == "-" then
      result = result + sign * number
      number = 0
      sign = ch == "+" and 1 or -1
    elseif ch == "(" then
      stack[#stack + 1] = result
      stack[#stack + 1] = sign
      result, number, sign = 0, 0, 1
    elseif ch == ")" then
      result = result + sign * number
      number = 0
      local prevSign = stack[#stack]
      stack[#stack] = nil
      local prevResult = stack[#stack]
      stack[#stack] = nil
      result = prevResult + prevSign * result
    end
  end
  return result + sign * number
end

local t = tonumber(io.read("*l"))
if t then
  local out = {}
  for i = 1, t do
    out[i] = tostring(calculate(io.read("*l") or ""))
  end
  io.write(table.concat(out, "\n"))
end
