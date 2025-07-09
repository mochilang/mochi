const customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}];
const orders = [{id: 100, customerId: 1, total: 250}, {id: 101, customerId: 3, total: 80}];
const result = (() => {
  const _tmp1 = [];
  for (const o of orders) {
    for (const c of customers) {
      if (!((o.customerId == c.id))) continue;
      _tmp1.push({orderId: o.id, customer: c, total: o.total});
    }
  }
  let res = _tmp1;
  return res;
})()
;
console.log("--- Left Join ---");
const _tmp2 = result;
for (const entry of (Array.isArray(_tmp2) ? _tmp2 : Object.keys(_tmp2))) {
  console.log("Order", entry.orderId, "customer", entry.customer, "total", entry.total);
}
