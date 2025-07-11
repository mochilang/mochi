const nums = [
  1,
  2,
  3
];
const letters = ["A", "B"];
const pairs = (() => {
  const _tmp1: Array<{ l: any; n: any }> = [];
  for (const n of nums) {
    for (const l of letters) {
      if (!(((n % 2) == 0))) continue;
      _tmp1.push({n: n, l: l});
    }
  }
  return res;
})()
;
console.log("--- Even pairs ---");
for (const p of pairs) {
  console.log(p.n, p.l);
}
