let people = [{name: "Alice", age: 30}, {name: "Bob", age: 15}, {name: "Charlie", age: 65}, {name: "Diana", age: 45}];
let adults = (() => {
  const _tmp1 = [];
  for (const person of people) {
    if (!((person.age >= 18))) continue;
    _tmp1.push({name: person.name, age: person.age, is_senior: (person.age >= 60)});
  }
  let res = _tmp1;
  return res;
})()
;
console.log("--- Adults ---");
for (const person of adults) {
  console.log(person.name, "is", person.age, (person.is_senior ? " (senior)" : ""));
}
