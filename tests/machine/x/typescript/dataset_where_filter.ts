const people = [{name: "Alice", age: 30}, {name: "Bob", age: 15}, {name: "Charlie", age: 65}, {name: "Diana", age: 45}];
const adults = (() => {
  const _tmp10 = [];
  for (const person of people) {
    if (!((person.age >= 18))) continue;
    _tmp10.push({name: person.name, age: person.age, is_senior: (person.age >= 60)});
  }
  let res = _tmp10;
  return res;
})()
;
console.log("--- Adults ---");
const _tmp11 = adults;
for (const person of (Array.isArray(_tmp11) ? _tmp11 : Object.keys(_tmp11))) {
  console.log(person.name, "is", person.age, (person.is_senior ? " (senior)" : ""));
}
