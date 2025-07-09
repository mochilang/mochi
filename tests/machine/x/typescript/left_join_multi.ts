let customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}];
let orders = [{id: 100, customerId: 1}, {id: 101, customerId: 2}];
let items = [{orderId: 100, sku: "a"}];
let result = (() => {
  const _tmp1 = [];
  for (const o of orders) {
    for (const c of customers) {
      if (!((o.customerId == c.id))) continue;
      for (const i of items) {
        if (!((o.id == i.orderId))) continue;
        _tmp1.push({orderId: o.id, name: c.name, item: i});
      }
    }
  }
  let res = _tmp1;
  return res;
})()
;
console.log("--- Left Join Multi ---");
const _tmp2 = result;
for (const r of (Array.isArray(_tmp2) ? _tmp2 : Object.keys(_tmp2))) {
  console.log(r.orderId, r.name, r.item);
}
