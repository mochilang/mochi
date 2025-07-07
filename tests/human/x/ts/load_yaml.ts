interface Person {
  name: string;
  age: number;
  email: string;
}
// Simulated load of YAML file
const people: Person[] = [
  { name: "Alice", age: 30, email: "alice@example.com" },
  { name: "Bob", age: 15, email: "bob@example.com" },
  { name: "Charlie", age: 20, email: "charlie@example.com" },
];
const adults = people
  .filter((p) => p.age >= 18)
  .map((p) => ({ name: p.name, email: p.email }));
for (const a of adults) {
  console.log(a.name, a.email);
}
