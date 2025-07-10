const products = [{name: "Laptop", price: 1500}, {name: "Smartphone", price: 900}, {name: "Tablet", price: 600}, {name: "Monitor", price: 300}, {name: "Keyboard", price: 100}, {name: "Mouse", price: 50}, {name: "Headphones", price: 200}];
const expensive = (() => {
  const _tmp8 = [];
  for (const p of products) {
    _tmp8.push({item: p, key: (-p.price)});
  }
  let res = _tmp8;
  res = res.sort((a,b)=> a.key < b.key ? -1 : a.key > b.key ? 1 : 0).map(x=>x.item);
  res = res.slice(1, (1 + 3));
  return res;
})()
;
console.log("--- Top products (excluding most expensive) ---");
const _tmp9 = expensive;
for (const item of (Array.isArray(_tmp9) ? _tmp9 : Object.keys(_tmp9))) {
  console.log(item.name, "costs $", item.price);
}
