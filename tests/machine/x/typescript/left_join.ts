const customers = [
  {id: 1, name: "Alice"},
  {id: 2, name: "Bob"}
];
const orders = [
  {
  id: 100,
  customerId: 1,
  total: 250
},
  {
  id: 101,
  customerId: 3,
  total: 80
}
];
const result = (() => {
  const _tmp1: Array<{ customer: any; orderId: any; total: any }> = [];
  for (const o of orders) {
    for (const c of customers) {
      if (!((o.customerId == c.id))) continue;
      _tmp1.push({
  orderId: o.id,
  customer: c,
  total: o.total
});
    }
  }
  return _tmp1;
})()
;
console.log("--- Left Join ---");
for (const entry of result) {
  console.log("Order", entry.orderId, "customer", entry.customer, "total", entry.total);
}
