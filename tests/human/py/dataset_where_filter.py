people = [
    {"name": "Alice", "age": 30},
    {"name": "Bob", "age": 15},
    {"name": "Charlie", "age": 65},
    {"name": "Diana", "age": 45},
]
adults = [
    {
        "name": p["name"],
        "age": p["age"],
        "is_senior": p["age"] >= 60,
    }
    for p in people
    if p["age"] >= 18
]
print("--- Adults ---")
for person in adults:
    suffix = " (senior)" if person["is_senior"] else ""
    print(person["name"], "is", person["age"], suffix)
