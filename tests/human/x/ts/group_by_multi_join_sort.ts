const nation = [
  { n_nationkey: 1, n_name: "BRAZIL" },
];
const customer = [
  {
    c_custkey: 1,
    c_name: "Alice",
    c_acctbal: 100.0,
    c_nationkey: 1,
    c_address: "123 St",
    c_phone: "123-456",
    c_comment: "Loyal",
  },
];
const orders = [
  { o_orderkey: 1000, o_custkey: 1, o_orderdate: "1993-10-15" },
  { o_orderkey: 2000, o_custkey: 1, o_orderdate: "1994-01-02" },
];
const lineitem = [
  { l_orderkey: 1000, l_returnflag: "R", l_extendedprice: 1000.0, l_discount: 0.1 },
  { l_orderkey: 2000, l_returnflag: "N", l_extendedprice: 500.0, l_discount: 0.0 },
];
const start_date = "1993-10-01";
const end_date = "1994-01-01";
interface Row {
  c_custkey: number;
  c_name: string;
  revenue: number;
  c_acctbal: number;
  n_name: string;
  c_address: string;
  c_phone: string;
  c_comment: string;
}
const temp: Record<string, { key: any; revenue: number }> = {};
for (const c of customer) {
  for (const o of orders) {
    if (o.o_custkey !== c.c_custkey) continue;
    if (o.o_orderdate < start_date || o.o_orderdate >= end_date) continue;
    for (const l of lineitem) {
      if (l.l_orderkey !== o.o_orderkey) continue;
      if (l.l_returnflag !== "R") continue;
      const n = nation.find((n) => n.n_nationkey === c.c_nationkey);
      if (!n) continue;
      const key = JSON.stringify({
        c_custkey: c.c_custkey,
        c_name: c.c_name,
        c_acctbal: c.c_acctbal,
        c_address: c.c_address,
        c_phone: c.c_phone,
        c_comment: c.c_comment,
        n_name: n.n_name,
      });
      const value = l.l_extendedprice * (1 - l.l_discount);
      const entry = temp[key] || { key: JSON.parse(key), revenue: 0 };
      entry.revenue += value;
      temp[key] = entry;
    }
  }
}
const result: Row[] = Object.values(temp)
  .sort((a, b) => b.revenue - a.revenue)
  .map(({ key, revenue }) => ({ ...key, revenue }));
console.log(result);
