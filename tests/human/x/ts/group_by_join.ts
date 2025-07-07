const customers = [
  { id: 1, name: "Alice" },
  { id: 2, name: "Bob" },
];
const orders = [
  { id: 100, customerId: 1 },
  { id: 101, customerId: 1 },
  { id: 102, customerId: 2 },
];
interface Group { name: string; count: number; }
const groups: Record<string, number> = {};
for (const o of orders) {
  const c = customers.find((x) => x.id === o.customerId);
  if (c) {
    groups[c.name] = (groups[c.name] || 0) + 1;
  }
}
const stats: Group[] = Object.entries(groups).map(([name, count]) => ({
  name,
  count,
}));
console.log("--- Orders per customer ---");
for (const s of stats) {
  console.log(s.name, "orders:", s.count);
}
