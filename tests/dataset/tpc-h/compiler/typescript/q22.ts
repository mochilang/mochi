function _json(v: any) {
  function _sort(x: any): any {
    if (Array.isArray(x)) return x.map(_sort);
    if (x && typeof x === "object") {
      const keys = Object.keys(x).sort();
      const o: any = {};
      for (const k of keys) o[k] = _sort(x[k]);
      return o;
    }
    return x;
  }
  return JSON.stringify(_sort(v));
}
function contains(a: any, b: any) {
  if (Array.isArray(a) || typeof a === "string") return a.includes(b);
  return Object.prototype.hasOwnProperty.call(a, b);
}
const customer = [
  {
  c_custkey: 1,
  c_phone: "13-123-4567",
  c_acctbal: 600
},
  {
  c_custkey: 2,
  c_phone: "31-456-7890",
  c_acctbal: 100
},
  {
  c_custkey: 3,
  c_phone: "30-000-0000",
  c_acctbal: 700
}
];
const orders = [{o_orderkey: 10, o_custkey: 2}];
const valid_codes = [
  "13",
  "31",
  "23",
  "29",
  "30",
  "18",
  "17"
];
const avg_balance = (customer.filter((c) => ((c.c_acctbal > 0) && contains(valid_codes, c.c_phone.substring(0, 2)))).map((c) => c.c_acctbal).reduce((a,b)=>a+b,0)/customer.filter((c) => ((c.c_acctbal > 0) && contains(valid_codes, c.c_phone.substring(0, 2)))).map((c) => c.c_acctbal).length);
const eligible_customers = customer.filter((c) => ((contains(valid_codes, c.c_phone.substring(0, 2)) && (c.c_acctbal > avg_balance)) && ((!orders.some((o) => (o.o_custkey == c.c_custkey)))))).map((c) => ({
  cntrycode: c.c_phone.substring(0, 2),
  c_acctbal: c.c_acctbal
}));
const groups = (() => {
  const _tmp1: any[] = [];
  const groups = {};
  for (const c of eligible_customers) {
    const _k = JSON.stringify(c.cntrycode);
    let g = groups[_k];
    if (!g) { g = []; g.key = c.cntrycode; g.items = g; groups[_k] = g; }
    g.push(c);
  }
  for (const _k in groups) {
    const g = groups[_k];
    _tmp1.push(g);
  }
  return _tmp1;
})()
;
let tmp = [];
const _tmp2 = groups;
for (const g of (Array.isArray(_tmp2) || typeof _tmp2 === "string" ? _tmp2 : Object.keys(_tmp2))) {
  const total = (g.items.map((x) => x.c_acctbal).reduce((a,b)=>a+b,0));
  const row = {
    cntrycode: g.key,
    numcust: g.length,
    totacctbal: total
  };
  tmp = [...tmp, row];
}
const result = tmp.slice().sort((a,b)=> a.cntrycode < b.cntrycode ? -1 : a.cntrycode > b.cntrycode ? 1 : 0);
console.log(_json(result));
if (!(JSON.stringify(result) === JSON.stringify([
  {
  cntrycode: "13",
  numcust: 1,
  totacctbal: 600
},
  {
  cntrycode: "30",
  numcust: 1,
  totacctbal: 700
}
]))) { throw new Error("Q22 returns wealthy inactive customers by phone prefix failed"); }
