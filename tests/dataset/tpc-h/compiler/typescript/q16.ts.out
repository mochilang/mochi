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
const supplier = [
  {
  s_suppkey: 100,
  s_name: "AlphaSupply",
  s_address: "123 Hilltop",
  s_comment: "Reliable and efficient"
},
  {
  s_suppkey: 200,
  s_name: "BetaSupply",
  s_address: "456 Riverside",
  s_comment: "Known for Customer Complaints"
}
];
const part = [
  {
  p_partkey: 1,
  p_brand: "Brand#12",
  p_type: "SMALL ANODIZED",
  p_size: 5
},
  {
  p_partkey: 2,
  p_brand: "Brand#23",
  p_type: "MEDIUM POLISHED",
  p_size: 10
}
];
const partsupp = [
  {ps_partkey: 1, ps_suppkey: 100},
  {ps_partkey: 2, ps_suppkey: 200}
];
const excluded_suppliers = (() => {
  const _tmp1: any[] = [];
  for (const ps of partsupp) {
    for (const p of part) {
      if (!((p.p_partkey == ps.ps_partkey))) continue;
      if (!((((p.p_brand == "Brand#12") && p.p_type.includes("SMALL")) && (p.p_size == 5)))) continue;
      _tmp1.push(ps.ps_suppkey);
    }
  }
  return _tmp1;
})()
;
const result = supplier.slice().sort((a,b)=> a.s_name < b.s_name ? -1 : a.s_name > b.s_name ? 1 : 0).filter((s) => (((!(contains(excluded_suppliers, s.s_suppkey))) && ((!s.s_comment.includes("Customer")))) && ((!s.s_comment.includes("Complaints"))))).map((s) => ({
  s_name: s.s_name,
  s_address: s.s_address
}));
console.log(_json(result));
if (!(JSON.stringify(result) === JSON.stringify([]))) { throw new Error("Q16 returns suppliers not linked to certain parts or complaints failed"); }
