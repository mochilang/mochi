data = [{"a": 1, "b": 2}, {"a": 1, "b": 1}, {"a": 0, "b": 5}]
sorted = sorted([x for x in data], key=lambda x: (x["a"], x["b"]))
print(sorted)
