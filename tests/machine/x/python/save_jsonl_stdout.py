import json
people = [
    {"name": "Alice", "age": 30},
    {"name": "Bob", "age": 25},
]
for p in people:
    print(json.dumps(p))
