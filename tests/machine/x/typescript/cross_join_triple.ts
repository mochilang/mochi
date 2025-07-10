const nums = [1, 2];
const letters = ["A", "B"];
const bools = [true, false];
const combos = (() => {
  const _tmp6 = [];
  for (const n of nums) {
    for (const l of letters) {
      for (const b of bools) {
        _tmp6.push({n: n, l: l, b: b});
      }
    }
  }
  let res = _tmp6;
  return res;
})()
;
console.log("--- Cross Join of three lists ---");
const _tmp7 = combos;
for (const c of (Array.isArray(_tmp7) ? _tmp7 : Object.keys(_tmp7))) {
  console.log(c.n, c.l, c.b);
}
