local math = { sqrt = math.sqrt, pow = math.pow, sin = math.sin, log = math.log, pi = math.pi, e = math.exp(1) }
r = 3.0
area = (math.pi * math.pow(r, 2.0))
root = math.sqrt(49.0)
sin45 = math.sin((math.pi / 4.0))
log_e = math.log(math.e)
print("Circle area with r =", r, "=>", area)
print("Square root of 49:", root)
print("sin(π/4):", sin45)
print("log(e):", log_e)
