# Translation of exists_builtin.mochi to Python

data = [1, 2]
flag = any(x == 1 for x in data)
print(flag)
