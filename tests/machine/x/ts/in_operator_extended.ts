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
console.log((Array.isArray(ys) || typeof ys === 'string' ? ys.includes(1) : Object.prototype.hasOwnProperty.call(ys, 1)));
console.log((Array.isArray(ys) || typeof ys === 'string' ? ys.includes(2) : Object.prototype.hasOwnProperty.call(ys, 2)));
let m = {a: 1};
console.log((Array.isArray(m) || typeof m === 'string' ? m.includes("a") : Object.prototype.hasOwnProperty.call(m, "a")));
console.log((Array.isArray(m) || typeof m === 'string' ? m.includes("b") : Object.prototype.hasOwnProperty.call(m, "b")));
let s = "hello";
console.log((Array.isArray(s) || typeof s === 'string' ? s.includes("ell") : Object.prototype.hasOwnProperty.call(s, "ell")));
console.log((Array.isArray(s) || typeof s === 'string' ? s.includes("foo") : Object.prototype.hasOwnProperty.call(s, "foo")));
