people = {{name = "Alice", age = 17, status = "minor"}, {name = "Bob", age = 25, status = "unknown"}, {name = "Charlie", age = 18, status = "unknown"}, {name = "Diana", age = 16, status = "minor"}};
for _i0 = 1, #people do
  local item = people[_i0];
  if (item.age >= 18) then
    item["status"] = "adult";
    item["age"] = (item.age + 1);
  end;
  people[_i0] = item;
end;
print("ok");
