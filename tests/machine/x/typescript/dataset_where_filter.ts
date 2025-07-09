const people = [{name: "Alice", age: 30}, {name: "Bob", age: 15}, {name: "Charlie", age: 65}, {name: "Diana", age: 45}];
const adults = (() => {
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
const _tmp2 = adults;
for (const person of (Array.isArray(_tmp2) ? _tmp2 : Object.keys(_tmp2))) {
  console.log(person.name, "is", person.age, (person.is_senior ? " (senior)" : ""));
}
