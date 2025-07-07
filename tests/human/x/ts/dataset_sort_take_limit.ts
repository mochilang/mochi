const products = [
  { name: "Laptop", price: 1500 },
  { name: "Smartphone", price: 900 },
  { name: "Tablet", price: 600 },
  { name: "Monitor", price: 300 },
  { name: "Keyboard", price: 100 },
  { name: "Mouse", price: 50 },
  { name: "Headphones", price: 200 },
];
const expensive = products
  .slice()
  .sort((a, b) => b.price - a.price)
  .slice(1, 4);
console.log("--- Top products (excluding most expensive) ---");
for (const item of expensive) {
  console.log(item.name, "costs $", item.price);
}
