const people = [
  {
  name: "Alice",
  age: 30,
  city: "Paris"
},
  {
  name: "Bob",
  age: 15,
  city: "Hanoi"
},
  {
  name: "Charlie",
  age: 65,
  city: "Paris"
},
  {
  name: "Diana",
  age: 45,
  city: "Hanoi"
},
  {
  name: "Eve",
  age: 70,
  city: "Paris"
},
  {
  name: "Frank",
  age: 22,
  city: "Hanoi"
}
];
const stats = (() => {
  const _tmp1: Array<{ avg_age: number; city: any; count: number }> = [];
  const groups = {};
  for (const person of people) {
    const _k = JSON.stringify(person.city);
    let g = groups[_k];
    if (!g) { g = []; g.key = person.city; g.items = g; groups[_k] = g; }
    g.push(person);
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push({
  city: g.key,
  count: g.length,
  avg_age: (g.map((p) => p.age).reduce((a,b)=>a+b,0)/g.map((p) => p.age).length)
});
  }
  return _tmp1;
})()
;
console.log("--- People grouped by city ---");
for (const s of stats) {
  console.log(s.city, ": count =", s.count, ", avg_age =", s.avg_age);
}
