const people = [
  { name: "Alice", age: 30, city: "Paris" },
  { name: "Bob", age: 15, city: "Hanoi" },
  { name: "Charlie", age: 65, city: "Paris" },
  { name: "Diana", age: 45, city: "Hanoi" },
  { name: "Eve", age: 70, city: "Paris" },
  { name: "Frank", age: 22, city: "Hanoi" },
];
interface Stat {
  city: string;
  count: number;
  avg_age: number;
}
const groups: Record<string, { totalAge: number; count: number }> = {};
for (const p of people) {
  const g = groups[p.city] || { totalAge: 0, count: 0 };
  g.totalAge += p.age;
  g.count += 1;
  groups[p.city] = g;
}
const stats: Stat[] = Object.entries(groups).map(([city, g]) => ({
  city,
  count: g.count,
  avg_age: g.totalAge / g.count,
}));
console.log("--- People grouped by city ---");
for (const s of stats) {
  console.log(`${s.city}: count = ${s.count}, avg_age = ${s.avg_age}`);
}
