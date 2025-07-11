const data = [
  {a: 1, b: 2},
  {a: 1, b: 1},
  {a: 0, b: 5}
];
const sorted = data.slice().sort((a,b)=> {a: a.a, b: a.b} < {a: b.a, b: b.b} ? -1 : {a: a.a, b: a.b} > {a: b.a, b: b.b} ? 1 : 0);
console.log(sorted);
