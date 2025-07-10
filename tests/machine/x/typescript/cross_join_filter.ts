const nums = [1, 2, 3];
const letters = ["A", "B"];
const pairs = (() => {
  const _tmp4 = [];
  for (const n of nums) {
    for (const l of letters) {
      if (!(((n % 2) == 0))) continue;
      _tmp4.push({n: n, l: l});
    }
  }
  let res = _tmp4;
  return res;
})()
;
console.log("--- Even pairs ---");
const _tmp5 = pairs;
for (const p of (Array.isArray(_tmp5) ? _tmp5 : Object.keys(_tmp5))) {
  console.log(p.n, p.l);
}
