data = [
    {"tag": "a", "val": 1},
    {"tag": "a", "val": 2},
    {"tag": "b", "val": 3},
]

# Group data by tag
groups = {}
for d in data:
    groups.setdefault(d["tag"], []).append(d)

tmp = []
for tag, items in groups.items():
    total = sum(x["val"] for x in items)
    tmp.append({"tag": tag, "total": total})

result = sorted(tmp, key=lambda r: r["tag"])
print(result)
