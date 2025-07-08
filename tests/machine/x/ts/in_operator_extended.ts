function contains(a: any, b: any) {
  if (Array.isArray(a) || typeof a === "string") return a.includes(b);
  return Object.prototype.hasOwnProperty.call(a, b);
}
let xs = [1, 2, 3];
let ys = (() => {
  const _tmp1 = [];
  for (const x of xs) {
    if (!(((x % 2) == 1))) continue;
    _tmp1.push(x);
  }
  let res = _tmp1;
  return res;
})()
;
console.log(contains(ys, 1));
console.log(contains(ys, 2));
let m = {a: 1};
console.log(contains(m, "a"));
console.log(contains(m, "b"));
let s = "hello";
console.log(contains(s, "ell"));
console.log(contains(s, "foo"));
