local Counter = {n = 0}
local function inc(c)
  c.n = c.n + 1
end
local c = {n = 0}
setmetatable(c, {__index = Counter})
inc(c)
print(c.n)
