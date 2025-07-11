function contains(a: any, b: any) {
  if (Array.isArray(a) || typeof a === "string") return a.includes(b);
  return Object.prototype.hasOwnProperty.call(a, b);
}
const nums = [
  1,
  2,
  3
];
console.log(contains(nums, 2));
console.log(contains(nums, 4));
