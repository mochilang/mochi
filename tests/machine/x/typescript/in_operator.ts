function contains(a: any, b: any) {
  if (Array.isArray(a) || typeof a === "string") return a.includes(b);
  return Object.prototype.hasOwnProperty.call(a, b);
}
const xs = [
  1,
  2,
  3
];
console.log(contains(xs, 2));
console.log((!(contains(xs, 5))));
