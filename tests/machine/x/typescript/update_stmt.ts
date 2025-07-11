interface Person {
  name: string;
  age: number;
  status: string;
}
const people: Person[] = [
  {name: "Alice", age: 17, status: "minor"},
  {name: "Bob", age: 25, status: "unknown"},
  {name: "Charlie", age: 18, status: "unknown"},
  {name: "Diana", age: 16, status: "minor"}
];
for (let i = 0; i < people.length; i++) {
  let _tmp1 = people[i];
  let age = _tmp1.age;
  let status = _tmp1.status;
  if ((age >= 18)) {
    _tmp1.status = "adult";
    _tmp1.age = (age + 1);
  }
  people[i] = _tmp1;
}
if (!(JSON.stringify(people) === JSON.stringify([
  {name: "Alice", age: 17, status: "minor"},
  {name: "Bob", age: 26, status: "adult"},
  {name: "Charlie", age: 19, status: "adult"},
  {name: "Diana", age: 16, status: "minor"}
]))) { throw new Error("update adult status failed"); }
console.log("ok");
