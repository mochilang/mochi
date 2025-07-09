const data = [{a: 1, b: 2}, {a: 1, b: 1}, {a: 0, b: 5}];
const sorted = (() => {
  const _tmp1 = [];
  for (const x of data) {
    _tmp1.push({item: x, key: {a: x.a, b: x.b}});
  }
  let res = _tmp1;
  res = res.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return res;
})()
;
console.log(sorted);
