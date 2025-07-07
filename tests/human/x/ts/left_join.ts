const customers = [
  { id: 1, name: "Alice" },
  { id: 2, name: "Bob" },
];
const orders = [
  { id: 100, customerId: 1, total: 250 },
  { id: 101, customerId: 3, total: 80 },
];
interface Entry {
  orderId: number;
  customer: { id: number; name: string } | undefined;
  total: number;
}
const result: Entry[] = orders.map((o) => ({
  orderId: o.id,
  customer: customers.find((c) => c.id === o.customerId),
  total: o.total,
}));
console.log("--- Left Join ---");
for (const entry of result) {
  console.log("Order", entry.orderId, "customer", entry.customer, "total", entry.total);
}
