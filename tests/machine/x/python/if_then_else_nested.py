x: int = 8
msg: str = "big" if x > 10 else "medium" if x > 5 else "small"
print(msg)
