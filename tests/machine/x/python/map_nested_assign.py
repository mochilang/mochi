from __future__ import annotations

data = {"outer": {"inner": 1}}
data["outer"]["inner"] = 2
print(data["outer"]["inner"])
