const customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}, {id: 3, name: "Charlie"}, {id: 4, name: "Diana"}];
const orders = [{id: 100, customerId: 1, total: 250}, {id: 101, customerId: 2, total: 125}, {id: 102, customerId: 1, total: 300}];
const result = (() => {
  const _tmp1 = [];
  for (const c of customers) {
    for (const o of orders) {
      if (!((o.customerId == c.id))) continue;
      _tmp1.push({customerName: c.name, order: o});
    }
  }
  let res = _tmp1;
  return res;
})()
;
console.log("--- Right Join using syntax ---");
const _tmp2 = result;
for (const entry of (Array.isArray(_tmp2) ? _tmp2 : Object.keys(_tmp2))) {
  if (entry.order) {
    console.log("Customer", entry.customerName, "has order", entry.order.id, "- $", entry.order.total);
  } else {
    console.log("Customer", entry.customerName, "has no orders");
  }
}
