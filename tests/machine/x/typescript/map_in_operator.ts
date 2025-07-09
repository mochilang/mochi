function contains(a: any, b: any) {
  if (Array.isArray(a) || typeof a === "string") return a.includes(b);
  return Object.prototype.hasOwnProperty.call(a, b);
}
const m = {1: "a", 2: "b"};
console.log(contains(m, 1));
console.log(contains(m, 3));
