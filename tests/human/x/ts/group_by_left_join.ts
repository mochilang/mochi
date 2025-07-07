const customers = [
  { id: 1, name: "Alice" },
  { id: 2, name: "Bob" },
  { id: 3, name: "Charlie" }, // No orders
];
const orders = [
  { id: 100, customerId: 1 },
  { id: 101, customerId: 1 },
  { id: 102, customerId: 2 },
];
interface Stat { name: string; count: number; }
const stats: Stat[] = [];
for (const c of customers) {
  const count = orders.filter((o) => o.customerId === c.id).length;
  stats.push({ name: c.name, count });
}
console.log("--- Group Left Join ---");
for (const s of stats) {
  console.log(s.name, "orders:", s.count);
}
