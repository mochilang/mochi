const data = [
  { a: 1, b: 2 },
  { a: 1, b: 1 },
  { a: 0, b: 5 },
];
const sorted = data
  .slice()
  .sort((x, y) => {
    if (x.a !== y.a) return x.a - y.a;
    return x.b - y.b;
  });
console.log(sorted);
