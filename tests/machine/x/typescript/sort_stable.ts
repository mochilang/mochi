const items = [
  {n: 1, v: "a"},
  {n: 1, v: "b"},
  {n: 2, v: "c"}
];
const result = items.slice().sort((a,b)=> a.n < b.n ? -1 : a.n > b.n ? 1 : 0).map((i) => i.v);
console.log(result);
