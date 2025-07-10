const customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}];
const orders = [{id: 100, customerId: 1, total: 250}, {id: 101, customerId: 3, total: 80}];
const result = (() => {
  const _tmp44 = [];
  for (const o of orders) {
    for (const c of customers) {
      if (!((o.customerId == c.id))) continue;
      _tmp44.push({orderId: o.id, customer: c, total: o.total});
    }
  }
  let res = _tmp44;
  return res;
})()
;
console.log("--- Left Join ---");
const _tmp45 = result;
for (const entry of (Array.isArray(_tmp45) ? _tmp45 : Object.keys(_tmp45))) {
  console.log("Order", entry.orderId, "customer", entry.customer, "total", entry.total);
}
