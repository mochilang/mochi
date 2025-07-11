const products = [
  {name: "Laptop", price: 1500},
  {name: "Smartphone", price: 900},
  {name: "Tablet", price: 600},
  {name: "Monitor", price: 300},
  {name: "Keyboard", price: 100},
  {name: "Mouse", price: 50},
  {name: "Headphones", price: 200}
];
const expensive = products.slice().sort((a,b)=> (-a.price) < (-b.price) ? -1 : (-a.price) > (-b.price) ? 1 : 0).slice(1, (1 + 3));
console.log("--- Top products (excluding most expensive) ---");
for (const item of expensive) {
  console.log(item.name, "costs $", item.price);
}
