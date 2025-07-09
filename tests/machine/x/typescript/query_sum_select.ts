const nums = [1, 2, 3];
const result = (() => {
  let _tmp1 = 0;
  for (const n of nums) {
    if (!((n > 1))) continue;
    _tmp1 += n;
  }
  let res = _tmp1;
  return res;
})()
;
console.log(result);
