type Counter = { n: number };
function inc(c: Counter): void {
  c.n = c.n + 1;
}
const c: Counter = { n: 0 };
inc(c);
console.log(c.n);
