const people = [
  { name: "Alice", age: 30 },
  { name: "Bob", age: 15 },
  { name: "Charlie", age: 65 },
  { name: "Diana", age: 45 },
];
const adults = people
  .filter((p) => p.age >= 18)
  .map((p) => ({
    name: p.name,
    age: p.age,
    is_senior: p.age >= 60,
  }));
console.log("--- Adults ---");
for (const person of adults) {
  const suffix = person.is_senior ? " (senior)" : "";
  console.log(person.name, "is", person.age, suffix);
}
