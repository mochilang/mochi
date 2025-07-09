const items = [{cat: "a", val: 3}, {cat: "a", val: 1}, {cat: "b", val: 5}, {cat: "b", val: 2}];
const grouped = (() => {
  const groups = {};
  for (const i of items) {
    const _k = JSON.stringify(i.cat);
    let g = groups[_k];
    if (!g) { g = []; g.key = i.cat; g.items = g; groups[_k] = g; }
    g.push(i);
  }
  let res = [];
  for (const _k in groups) {
    const g = groups[_k];
    res.push({item: {cat: g.key, total: ((() => {
  const _tmp1 = [];
  for (const x of g) {
    _tmp1.push(x.val);
  }
  let res = _tmp1;
  return res;
})()
.reduce((a,b)=>a+b,0))}, key: (-((() => {
  const _tmp2 = [];
  for (const x of g) {
    _tmp2.push(x.val);
  }
  let res = _tmp2;
  return res;
})()
.reduce((a,b)=>a+b,0)))});
  }
  res = res.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return res;
})()
;
console.log(grouped);
