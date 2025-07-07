const items = [
  { n: 1, v: "a" },
  { n: 1, v: "b" },
  { n: 2, v: "c" }
];
items.sort((a, b) => a.n - b.n);
console.log(items.map(i => i.v).join(' '));
