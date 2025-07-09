const customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}, {id: 3, name: "Charlie"}];
const orders = [{id: 100, customerId: 1, total: 250}, {id: 101, customerId: 2, total: 125}, {id: 102, customerId: 1, total: 300}];
const result = (() => {
  const _tmp1 = [];
  for (const o of orders) {
    for (const c of customers) {
      _tmp1.push({orderId: o.id, orderCustomerId: o.customerId, pairedCustomerName: c.name, orderTotal: o.total});
    }
  }
  let res = _tmp1;
  return res;
})()
;
console.log("--- Cross Join: All order-customer pairs ---");
const _tmp2 = result;
for (const entry of (Array.isArray(_tmp2) ? _tmp2 : Object.keys(_tmp2))) {
  console.log("Order", entry.orderId, "(customerId:", entry.orderCustomerId, ", total: $", entry.orderTotal, ") paired with", entry.pairedCustomerName);
}
