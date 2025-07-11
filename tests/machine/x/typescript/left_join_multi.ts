const customers = [
  {id: 1, name: "Alice"},
  {id: 2, name: "Bob"}
];
const orders = [
  {id: 100, customerId: 1},
  {id: 101, customerId: 2}
];
const items = [{orderId: 100, sku: "a"}];
const result = (() => {
  const _tmp1: Array<{ item: any; name: any; orderId: any }> = [];
  for (const o of orders) {
    for (const c of customers) {
      if (!((o.customerId == c.id))) continue;
      for (const i of items) {
        if (!((o.id == i.orderId))) continue;
        _tmp1.push({
  orderId: o.id,
  name: c.name,
  item: i
});
      }
    }
  }
  return _tmp1;
})()
;
console.log("--- Left Join Multi ---");
for (const r of result) {
  console.log(r.orderId, r.name, r.item);
}
