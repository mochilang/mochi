local math = { sqrt = math.sqrt, pow = math.pow, sin = math.sin, log = math.log, pi = math.pi, e = math.exp(1) }
r = 3.0
area = (math.pi * math.pow(r, 2.0))
root = math.sqrt(49.0)
sin45 = math.sin((math.pi / 4.0))
log_e = math.log(math.e)
print(tostring("Circle area with r =") .. " " .. tostring(r) .. " " .. tostring("=>") .. " " .. tostring(area))
print(tostring("Square root of 49:") .. " " .. tostring(root))
print(tostring("sin(π/4):") .. " " .. tostring(sin45))
print(tostring("log(e):") .. " " .. tostring(log_e))
