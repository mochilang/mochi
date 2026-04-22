local function count_digit_one(n)
  local total, m = 0, 1
  while m <= n do
    local high = math.floor(n / (m * 10))
    local cur = math.floor(n / m) % 10
    local low = n % m
    if cur == 0 then
      total = total + high * m
    elseif cur == 1 then
      total = total + high * m + low + 1
    else
      total = total + (high + 1) * m
    end
    m = m * 10
  end
  return total
end

local t = tonumber(io.read("*l"))
if t then
  local out = {}
  for i = 1, t do
    out[i] = tostring(count_digit_one(tonumber(io.read("*l")) or 0))
  end
  io.write(table.concat(out, "\n"))
end
