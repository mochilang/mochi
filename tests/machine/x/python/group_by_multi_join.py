nations = [
    {"id": 1, "name": "A"},
    {"id": 2, "name": "B"},
]
suppliers = [
    {"id": 1, "nation": 1},
    {"id": 2, "nation": 2},
]
partsupp = [
    {"part": 100, "supplier": 1, "cost": 10.0, "qty": 2},
    {"part": 100, "supplier": 2, "cost": 20.0, "qty": 1},
    {"part": 200, "supplier": 1, "cost": 5.0, "qty": 3},
]

filtered = []
for ps in partsupp:
    s = next(s for s in suppliers if s["id"] == ps["supplier"])
    n = next(n for n in nations if n["id"] == s["nation"])
    if n["name"] == "A":
        filtered.append({"part": ps["part"], "value": ps["cost"] * ps["qty"]})

grouped = {}
for x in filtered:
    grouped[x["part"]] = grouped.get(x["part"], 0) + x["value"]

result = [{"part": part, "total": total} for part, total in grouped.items()]
print(result)
