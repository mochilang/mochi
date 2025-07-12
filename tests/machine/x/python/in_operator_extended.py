xs = [1, 2, 3]
ys = [x for x in xs if x % 2 == 1]
print(str(1 in ys).lower())
print(str(2 in ys).lower())
m = {"a": 1}
print(str("a" in m).lower())
print(str("b" in m).lower())
s = "hello"
print(str("ell" in s).lower())
print(str("foo" in s).lower())
