const nums = [1, 2, 3];
const letters = ["A", "B"];
const pairs = (() => {
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
const _tmp2 = pairs;
for (const p of (Array.isArray(_tmp2) ? _tmp2 : Object.keys(_tmp2))) {
  console.log(p.n, p.l);
}
