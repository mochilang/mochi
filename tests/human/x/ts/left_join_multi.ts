const customers = [
  { id: 1, name: "Alice" },
  { id: 2, name: "Bob" },
];
const orders = [
  { id: 100, customerId: 1 },
  { id: 101, customerId: 2 },
];
const items = [
  { orderId: 100, sku: "a" },
];
interface Row { orderId: number; name: string; item: { orderId: number; sku: string } | undefined; }
const result: Row[] = [];
for (const o of orders) {
  const c = customers.find((cu) => cu.id === o.customerId);
  const i = items.find((it) => it.orderId === o.id);
  if (c) {
    result.push({ orderId: o.id, name: c.name, item: i });
  }
}
console.log("--- Left Join Multi ---");
for (const r of result) {
  console.log(r.orderId, r.name, r.item);
}
