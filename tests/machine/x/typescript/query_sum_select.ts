const nums = [1, 2, 3];
const result = (() => {
  let _tmp58 = 0;
  for (const n of nums) {
    if (!((n > 1))) continue;
    _tmp58 += n;
  }
  let res = _tmp58;
  return res;
})()
;
console.log(result);
