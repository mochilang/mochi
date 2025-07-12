data: dict[str, dict[str, int]] = {"outer": {"inner": 1}}
data["outer"]["inner"] = 2
print(data["outer"]["inner"])
