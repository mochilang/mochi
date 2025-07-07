items = [
    {"n": 1, "v": "a"},
    {"n": 1, "v": "b"},
    {"n": 2, "v": "c"},
]
result = [i['v'] for i in sorted(items, key=lambda x: x['n'])]
print(result)
