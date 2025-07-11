const items = [
  {
  cat: "a",
  val: 10,
  flag: true
},
  {
  cat: "a",
  val: 5,
  flag: false
},
  {
  cat: "b",
  val: 20,
  flag: true
}
];
const result = (() => {
  const _tmp1: Array<{ cat: any; share: any }> = [];
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
  share: ((g.map((x) => (x.flag ? x.val : 0)).reduce((a,b)=>a+b,0)) / (g.map((x) => x.val).reduce((a,b)=>a+b,0)))
}, key: g.key});
  }
  _tmp1 = _tmp1.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  return _tmp1;
})()
;
console.log(result);
