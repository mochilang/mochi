const data = [
  {a: 1, b: 2},
  {a: 1, b: 1},
  {a: 0, b: 5}
];
const sorted = data.slice().sort((a,b)=> JSON.stringify({a: a.a, b: a.b}) < JSON.stringify({a: b.a, b: b.b}) ? -1 : JSON.stringify({a: a.a, b: a.b}) > JSON.stringify({a: b.a, b: b.b}) ? 1 : 0);
console.log(sorted);
