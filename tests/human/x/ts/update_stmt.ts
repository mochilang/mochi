interface Person {
  name: string;
  age: number;
  status: string;
}
const people: Person[] = [
  { name: "Alice", age: 17, status: "minor" },
  { name: "Bob", age: 25, status: "unknown" },
  { name: "Charlie", age: 18, status: "unknown" },
  { name: "Diana", age: 16, status: "minor" },
];
for (const p of people) {
  if (p.age >= 18) {
    p.status = "adult";
    p.age = p.age + 1;
  }
}
console.log("ok");
console.log(JSON.stringify(people));
