const people = [
  {name: "Alice", age: 30},
  {name: "Bob", age: 15},
  {name: "Charlie", age: 65},
  {name: "Diana", age: 45}
];
const adults = people.filter((person) => person.age >= 18).map((person) => ({
  name: person.name,
  age: person.age,
  is_senior: person.age >= 60,
}));
console.log("--- Adults ---");
for (const person of adults) {
  console.log(person.name, "is", person.age, person.is_senior ? " (senior)" : "");
}
