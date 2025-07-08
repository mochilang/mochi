people = [{"name": "Alice", "age": 30}, {"name": "Bob", "age": 15}, {"name": "Charlie", "age": 65}, {"name": "Diana", "age": 45}]
adults = [{"name": person["name"], "age": person["age"], "is_senior": person["age"] >= 60} for person in people if person["age"] >= 18]
print("--- Adults ---")
for person in adults:
    print(person["name"], "is", person["age"], (" (senior)" if person["is_senior"] else ""))
