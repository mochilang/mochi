const people = [{name: "Alice", age: 30, city: "Paris"}, {name: "Bob", age: 15, city: "Hanoi"}, {name: "Charlie", age: 65, city: "Paris"}, {name: "Diana", age: 45, city: "Hanoi"}, {name: "Eve", age: 70, city: "Paris"}, {name: "Frank", age: 22, city: "Hanoi"}];
const stats = (() => {
  const groups = {};
  for (const person of people) {
    const _k = JSON.stringify(person.city);
    let g = groups[_k];
    if (!g) { g = []; g.key = person.city; g.items = g; groups[_k] = g; }
    g.push(person);
  }
  let res = [];
  for (const _k in groups) {
    const g = groups[_k];
    res.push({city: g.key, count: g.length, avg_age: ((() => {
  const _tmp14 = [];
  for (const p of g) {
    _tmp14.push(p.age);
  }
  let res = _tmp14;
  return res;
})()
.reduce((a,b)=>a+b,0)/(() => {
  const _tmp14 = [];
  for (const p of g) {
    _tmp14.push(p.age);
  }
  let res = _tmp14;
  return res;
})()
.length)});
  }
  return res;
})()
;
console.log("--- People grouped by city ---");
const _tmp16 = stats;
for (const s of (Array.isArray(_tmp16) ? _tmp16 : Object.keys(_tmp16))) {
  console.log(s.city, ": count =", s.count, ", avg_age =", s.avg_age);
}
