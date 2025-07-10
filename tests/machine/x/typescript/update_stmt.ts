type Person = { name: any; age: any; status: any; };
const people = [{name: "Alice", age: 17, status: "minor"}, {name: "Bob", age: 25, status: "unknown"}, {name: "Charlie", age: 18, status: "unknown"}, {name: "Diana", age: 16, status: "minor"}];
for (let i = 0; i < people.length; i++) {
  let _tmp64 = people[i];
  let status = _tmp64.status;
  let age = _tmp64.age;
  if ((age >= 18)) {
    _tmp64.status = "adult";
    _tmp64.age = (age + 1);
  }
  people[i] = _tmp64;
}
if (!(JSON.stringify(people) === JSON.stringify([{name: "Alice", age: 17, status: "minor"}, {name: "Bob", age: 26, status: "adult"}, {name: "Charlie", age: 19, status: "adult"}, {name: "Diana", age: 16, status: "minor"}]))) { throw new Error("update adult status failed"); }
console.log("ok");
