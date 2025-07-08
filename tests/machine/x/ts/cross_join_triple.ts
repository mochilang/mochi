let nums = [1, 2];
let letters = ["A", "B"];
let bools = [true, false];
let combos = (() => {
  const _tmp1 = [];
  for (const n of nums) {
    for (const l of letters) {
      for (const b of bools) {
        _tmp1.push({n: n, l: l, b: b});
      }
    }
  }
  let res = _tmp1;
  return res;
})()
;
console.log("--- Cross Join of three lists ---");
for (const c of combos) {
  console.log(c.n, c.l, c.b);
}
