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
const part = [
  {
  p_partkey: 1,
  p_brand: "Brand#12",
  p_container: "SM BOX",
  p_size: 3
},
  {
  p_partkey: 2,
  p_brand: "Brand#23",
  p_container: "MED BOX",
  p_size: 5
},
  {
  p_partkey: 3,
  p_brand: "Brand#34",
  p_container: "LG BOX",
  p_size: 15
}
];
const lineitem = [
  {
  l_partkey: 1,
  l_quantity: 5,
  l_extendedprice: 1000,
  l_discount: 0.1,
  l_shipmode: "AIR",
  l_shipinstruct: "DELIVER IN PERSON"
},
  {
  l_partkey: 2,
  l_quantity: 15,
  l_extendedprice: 2000,
  l_discount: 0.05,
  l_shipmode: "AIR REG",
  l_shipinstruct: "DELIVER IN PERSON"
},
  {
  l_partkey: 3,
  l_quantity: 35,
  l_extendedprice: 1500,
  l_discount: 0,
  l_shipmode: "AIR",
  l_shipinstruct: "DELIVER IN PERSON"
}
];
const revenues = (() => {
  const _tmp1: any[] = [];
  for (const l of lineitem) {
    for (const p of part) {
      if (!((p.p_partkey == l.l_partkey))) continue;
      if (!((((((((((((p.p_brand == "Brand#12")) && (contains([
  "SM CASE",
  "SM BOX",
  "SM PACK",
  "SM PKG"
], p.p_container))) && (((l.l_quantity >= 1) && (l.l_quantity <= 11)))) && (((p.p_size >= 1) && (p.p_size <= 5))))) || ((((((p.p_brand == "Brand#23")) && (contains([
  "MED BAG",
  "MED BOX",
  "MED PKG",
  "MED PACK"
], p.p_container))) && (((l.l_quantity >= 10) && (l.l_quantity <= 20)))) && (((p.p_size >= 1) && (p.p_size <= 10)))))) || ((((((p.p_brand == "Brand#34")) && (contains([
  "LG CASE",
  "LG BOX",
  "LG PACK",
  "LG PKG"
], p.p_container))) && (((l.l_quantity >= 20) && (l.l_quantity <= 30)))) && (((p.p_size >= 1) && (p.p_size <= 15))))))) && contains(["AIR", "AIR REG"], l.l_shipmode)) && (l.l_shipinstruct == "DELIVER IN PERSON")))) continue;
      _tmp1.push((l.l_extendedprice * ((1 - l.l_discount))));
    }
  }
  return _tmp1;
})()
;
const result = (revenues.reduce((a,b)=>a+b,0));
console.log(_json(result));
if (!((result == 2800))) { throw new Error("Q19 returns total revenue from qualifying branded parts failed"); }
