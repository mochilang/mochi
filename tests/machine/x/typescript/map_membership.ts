function contains(a: any, b: any) {
  if (Array.isArray(a) || typeof a === "string") return a.includes(b);
  return Object.prototype.hasOwnProperty.call(a, b);
}
const m = {a: 1, b: 2};
console.log(contains(m, "a"));
console.log(contains(m, "c"));
