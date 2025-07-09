const items = [{cat: "a", val: 10, flag: true}, {cat: "a", val: 5, flag: false}, {cat: "b", val: 20, flag: true}];
const result = (() => {
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
    res.push({item: {cat: g.key, share: (((() => {
  const _tmp1 = [];
  for (const x of g) {
    _tmp1.push((x.flag ? x.val : 0));
  }
  let res = _tmp1;
  return res;
})()
.reduce((a,b)=>a+b,0)) / ((() => {
  const _tmp2 = [];
  for (const x of g) {
    _tmp2.push(x.val);
  }
  let res = _tmp2;
  return res;
})()
.reduce((a,b)=>a+b,0)))}, key: g.key});
  }
  res = res.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return res;
})()
;
console.log(result);
