const items = [
  {cat: "a", val: 3},
  {cat: "a", val: 1},
  {cat: "b", val: 5},
  {cat: "b", val: 2}
];
const grouped = (() => {
  const _tmp1: Array<{ cat: any; total: number }> = [];
  const groups = {};
  for (const i of items) {
    const _k = JSON.stringify(i.cat);
    let g = groups[_k];
    if (!g) { g = []; g.key = i.cat; g.items = g; groups[_k] = g; }
    g.push(i);
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push({item: {
  cat: g.key,
  total: (g.map((x) => x.val).reduce((a,b)=>a+b,0))
}, key: (-(g.map((x) => x.val).reduce((a,b)=>a+b,0)))});
  }
  _tmp1 = _tmp1.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return _tmp1;
})()
;
console.log(grouped);
