items = [
    {"cat": "a", "val": 10, "flag": True},
    {"cat": "a", "val": 5, "flag": False},
    {"cat": "b", "val": 20, "flag": True},
]

# Group by category and compute conditional share
stats = {}
for itm in items:
    g = stats.setdefault(itm["cat"], {"num": 0, "den": 0})
    if itm["flag"]:
        g["num"] += itm["val"]
    g["den"] += itm["val"]

result = [
    {"cat": cat, "share": (g["num"] / g["den"] if g["den"] else 0)}
    for cat, g in stats.items()
]
result.sort(key=lambda r: r["cat"])
print(result)
