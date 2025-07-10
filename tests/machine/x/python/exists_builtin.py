from __future__ import annotations

data = [1, 2]
flag = len([x for x in data if x == 1]) > 0
print(str(flag).lower())
