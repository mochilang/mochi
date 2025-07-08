let nums = [1, 2, 3];
let letters = ["A", "B"];
let pairs = (() => {
  const _tmp1 = [];
  for (const n of nums) {
    for (const l of letters) {
      if (!(((n % 2) == 0))) continue;
      _tmp1.push({n: n, l: l});
    }
  }
  let res = _tmp1;
  return res;
})()
;
console.log("--- Even pairs ---");
for (const p of pairs) {
  console.log(p.n, p.l);
}
