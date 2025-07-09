const customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}, {id: 3, name: "Charlie"}];
const orders = [{id: 100, customerId: 1, total: 250}, {id: 101, customerId: 2, total: 125}, {id: 102, customerId: 1, total: 300}, {id: 103, customerId: 4, total: 80}];
const result = (() => {
  const _tmp1 = [];
  for (const o of orders) {
    for (const c of customers) {
      if (!((o.customerId == c.id))) continue;
      _tmp1.push({orderId: o.id, customerName: c.name, total: o.total});
    }
  }
  let res = _tmp1;
  return res;
})()
;
console.log("--- Orders with customer info ---");
const _tmp2 = result;
for (const entry of (Array.isArray(_tmp2) ? _tmp2 : Object.keys(_tmp2))) {
  console.log("Order", entry.orderId, "by", entry.customerName, "- $", entry.total);
}
