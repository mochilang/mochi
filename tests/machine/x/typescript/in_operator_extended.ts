function contains(a: any, b: any) {
  if (Array.isArray(a) || typeof a === "string") return a.includes(b);
  return Object.prototype.hasOwnProperty.call(a, b);
}
const xs = [
  1,
  2,
  3
];
const ys = xs.filter((x) => ((x % 2) == 1));
console.log(contains(ys, 1));
console.log(contains(ys, 2));
const m = {a: 1};
console.log(contains(m, "a"));
console.log(contains(m, "b"));
const s = "hello";
console.log(contains(s, "ell"));
console.log(contains(s, "foo"));
