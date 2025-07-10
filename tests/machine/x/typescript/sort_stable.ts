const items = [{n: 1, v: "a"}, {n: 1, v: "b"}, {n: 2, v: "c"}];
const result = (() => {
  const _tmp62 = [];
  for (const i of items) {
    _tmp62.push({item: i.v, key: i.n});
  }
  let res = _tmp62;
  res = res.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return res;
})()
;
console.log(result);
