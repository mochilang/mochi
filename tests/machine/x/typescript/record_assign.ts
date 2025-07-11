type Counter = { n: number; };
function inc(c) {
  c.n = (c.n + 1);
}
let c = {n: 0};
inc(c);
console.log(c.n);
