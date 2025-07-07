const people = [
  { name: "Alice", city: "Paris" },
  { name: "Bob", city: "Hanoi" },
  { name: "Charlie", city: "Paris" },
  { name: "Diana", city: "Hanoi" },
  { name: "Eve", city: "Paris" },
  { name: "Frank", city: "Hanoi" },
  { name: "George", city: "Paris" },
];
const groups: Record<string, number> = {};
for (const p of people) {
  groups[p.city] = (groups[p.city] || 0) + 1;
}
const big = Object.entries(groups)
  .filter(([_, count]) => count >= 4)
  .map(([city, count]) => ({ city, num: count }));
console.log(JSON.stringify(big));
