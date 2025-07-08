let nums = [1, 2, 3];
let result = (() => {
  const _tmp1 = [];
  for (const n of nums) {
    if (!((n > 1))) continue;
    _tmp1.push((n.reduce((a,b)=>a+b,0)));
  }
  let res = _tmp1;
  return res;
})()
;
console.log(result);
