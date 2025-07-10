const customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}];
const orders = [{id: 100, customerId: 1}, {id: 101, customerId: 2}];
const items = [{orderId: 100, sku: "a"}];
const result = (() => {
  const _tmp46 = [];
  for (const o of orders) {
    for (const c of customers) {
      if (!((o.customerId == c.id))) continue;
      for (const i of items) {
        if (!((o.id == i.orderId))) continue;
        _tmp46.push({orderId: o.id, name: c.name, item: i});
      }
    }
  }
  let res = _tmp46;
  return res;
})()
;
console.log("--- Left Join Multi ---");
const _tmp47 = result;
for (const r of (Array.isArray(_tmp47) ? _tmp47 : Object.keys(_tmp47))) {
  console.log(r.orderId, r.name, r.item);
}
