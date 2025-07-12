data: list[int] = [1, 2]
flag: bool = len([x for x in data if x == 1]) > 0
print(str(flag).lower())
