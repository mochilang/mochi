const customers = [
  { id: 1, name: "Alice" },
  { id: 2, name: "Bob" },
  { id: 3, name: "Charlie" },
];
const orders = [
  { id: 100, customerId: 1, total: 250 },
  { id: 101, customerId: 2, total: 125 },
  { id: 102, customerId: 1, total: 300 },
  { id: 103, customerId: 4, total: 80 },
];
const result = [] as Array<{ orderId: number; customerName: string; total: number }>;
for (const o of orders) {
  const c = customers.find((cu) => cu.id === o.customerId);
  if (c) {
    result.push({ orderId: o.id, customerName: c.name, total: o.total });
  }
}
console.log("--- Orders with customer info ---");
for (const entry of result) {
  console.log("Order", entry.orderId, "by", entry.customerName, "- $", entry.total);
}
