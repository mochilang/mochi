const customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}, {id: 3, name: "Charlie"}];
const orders = [{id: 100, customerId: 1, total: 250}, {id: 101, customerId: 2, total: 125}, {id: 102, customerId: 1, total: 300}];
const result = (() => {
  const _tmp2 = [];
  for (const o of orders) {
    for (const c of customers) {
      _tmp2.push({orderId: o.id, orderCustomerId: o.customerId, pairedCustomerName: c.name, orderTotal: o.total});
    }
  }
  let res = _tmp2;
  return res;
})()
;
console.log("--- Cross Join: All order-customer pairs ---");
const _tmp3 = result;
for (const entry of (Array.isArray(_tmp3) ? _tmp3 : Object.keys(_tmp3))) {
  console.log("Order", entry.orderId, "(customerId:", entry.orderCustomerId, ", total: $", entry.orderTotal, ") paired with", entry.pairedCustomerName);
}
