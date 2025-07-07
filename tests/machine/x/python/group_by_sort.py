items = [
    {"cat": "a", "val": 3},
    {"cat": "a", "val": 1},
    {"cat": "b", "val": 5},
    {"cat": "b", "val": 2},
]

groups = {}
for i in items:
    groups.setdefault(i["cat"], []).append(i["val"])

result = [
    {"cat": cat, "total": sum(vals)}
    for cat, vals in groups.items()
]
result.sort(key=lambda r: -r["total"])
print(result)
