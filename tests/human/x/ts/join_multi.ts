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
  { orderId: 101, sku: "b" },
];
const result: Array<{ name: string; sku: string }> = [];
for (const o of orders) {
  const c = customers.find((cu) => cu.id === o.customerId);
  const i = items.find((it) => it.orderId === o.id);
  if (c && i) {
    result.push({ name: c.name, sku: i.sku });
  }
}
console.log("--- Multi Join ---");
for (const r of result) {
  console.log(r.name, "bought item", r.sku);
}
