let data = [1, 2];
let flag = ((() => {
  const _tmp1 = [];
  for (const x of data) {
    if (!((x == 1))) continue;
    _tmp1.push(x);
  }
  let res = _tmp1;
  return res;
})()
.length > 0);
console.log(flag);
