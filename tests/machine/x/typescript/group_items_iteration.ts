const data = [
  {tag: "a", val: 1},
  {tag: "a", val: 2},
  {tag: "b", val: 3}
];
const groups = (() => {
  const _tmp1: any[] = [];
  const groups = {};
  for (const d of data) {
    const _k = JSON.stringify(d.tag);
    let g = groups[_k];
    if (!g) { g = []; g.key = d.tag; g.items = g; groups[_k] = g; }
    g.push(d);
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push(g);
  }
  return _tmp1;
})()
;
let tmp = [];
for (const g of groups) {
  let total = 0;
  for (const x of g.items) {
    total = (total + x.val);
  }
  tmp = [...tmp, {tag: g.key, total: total}];
}
const result = tmp.slice().sort((a,b)=> a.tag < b.tag ? -1 : a.tag > b.tag ? 1 : 0);
console.log(result);
