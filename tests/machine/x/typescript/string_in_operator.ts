function contains(a: any, b: any) {
  if (Array.isArray(a) || typeof a === "string") return a.includes(b);
  return Object.prototype.hasOwnProperty.call(a, b);
}
let s = "catch";
console.log(contains(s, "cat"));
console.log(contains(s, "dog"));
