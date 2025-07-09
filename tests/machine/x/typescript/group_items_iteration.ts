const data = [{tag: "a", val: 1}, {tag: "a", val: 2}, {tag: "b", val: 3}];
const groups = (() => {
  const groups = {};
  for (const d of data) {
    const _k = JSON.stringify(d.tag);
    let g = groups[_k];
    if (!g) { g = []; g.key = d.tag; g.items = g; groups[_k] = g; }
    g.push(d);
  }
  let res = [];
  for (const _k in groups) {
    const g = groups[_k];
    res.push(g);
  }
  return res;
})()
;
let tmp = [];
const _tmp2 = groups;
for (const g of (Array.isArray(_tmp2) ? _tmp2 : Object.keys(_tmp2))) {
  let total = 0;
  const _tmp3 = g.items;
  for (const x of (Array.isArray(_tmp3) ? _tmp3 : Object.keys(_tmp3))) {
    total = (total + x.val);
  }
  tmp = [...tmp, {tag: g.key, total: total}];
}
const result = (() => {
  const _tmp4 = [];
  for (const r of tmp) {
    _tmp4.push({item: r, key: r.tag});
  }
  let res = _tmp4;
  res = res.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return res;
})()
;
console.log(result);
