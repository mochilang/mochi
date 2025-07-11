const customers = [
  {id: 1, name: "Alice"},
  {id: 2, name: "Bob"},
  {id: 3, name: "Charlie"},
  {id: 4, name: "Diana"}
];
const orders = [
  {
  id: 100,
  customerId: 1,
  total: 250
},
  {
  id: 101,
  customerId: 2,
  total: 125
},
  {
  id: 102,
  customerId: 1,
  total: 300
}
];
const result = (() => {
  const _tmp1: Array<{ customerName: any; order: any }> = [];
  for (const c of customers) {
    for (const o of orders) {
      if (!((o.customerId == c.id))) continue;
      _tmp1.push({customerName: c.name, order: o});
    }
  }
  return _tmp1;
})()
;
console.log("--- Right Join using syntax ---");
for (const entry of result) {
  if (entry.order) {
    console.log("Customer", entry.customerName, "has order", entry.order.id, "- $", entry.order.total);
  } else {
    console.log("Customer", entry.customerName, "has no orders");
  }
}
