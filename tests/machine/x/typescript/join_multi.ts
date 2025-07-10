const customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}];
const orders = [{id: 100, customerId: 1}, {id: 101, customerId: 2}];
const items = [{orderId: 100, sku: "a"}, {orderId: 101, sku: "b"}];
const result = (() => {
  const _tmp42 = [];
  for (const o of orders) {
    for (const c of customers) {
      if (!((o.customerId == c.id))) continue;
      for (const i of items) {
        if (!((o.id == i.orderId))) continue;
        _tmp42.push({name: c.name, sku: i.sku});
      }
    }
  }
  let res = _tmp42;
  return res;
})()
;
console.log("--- Multi Join ---");
const _tmp43 = result;
for (const r of (Array.isArray(_tmp43) ? _tmp43 : Object.keys(_tmp43))) {
  console.log(r.name, "bought item", r.sku);
}
