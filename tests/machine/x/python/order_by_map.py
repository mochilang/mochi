items = [
    {"a": 1, "b": 2},
    {"a": 1, "b": 1},
    {"a": 0, "b": 5},
]
# Sort by a then b
result = sorted(items, key=lambda x: (x['a'], x['b']))
print(result)
