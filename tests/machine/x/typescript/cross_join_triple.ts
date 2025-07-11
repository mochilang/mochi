const nums = [1, 2];
const letters = ["A", "B"];
const bools = [true, false];
const combos = (() => {
  const _tmp1: Array<{ b: any; l: any; n: any }> = [];
  for (const n of nums) {
    for (const l of letters) {
      for (const b of bools) {
        _tmp1.push({
  n: n,
  l: l,
  b: b
});
      }
    }
  }
  return res;
})()
;
console.log("--- Cross Join of three lists ---");
for (const c of combos) {
  console.log(c.n, c.l, c.b);
}
