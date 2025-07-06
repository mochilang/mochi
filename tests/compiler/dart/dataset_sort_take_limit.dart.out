Map<String, Function> _structParsers = {};

class Product {
  String name;
  int price;
  Product({required this.name, required this.price});
  factory Product.fromJson(Map<String,dynamic> m) {
    return Product(name: m['name'] as String, price: m['price'] as int);
  }
}

List<Product> products = [Product(name: "Laptop", price: 1500), Product(name: "Smartphone", price: 900), Product(name: "Tablet", price: 600), Product(name: "Monitor", price: 300), Product(name: "Keyboard", price: 100), Product(name: "Mouse", price: 50), Product(name: "Headphones", price: 200)];

List<Product> expensive = (() {
  var _res = [];
  for (var p in products) {
    _res.add(p);
  }
  var items = List.from(_res);
  items.sort((pA, pB) {
    var p = pA;
    var keyA = -p.price;
    p = pB;
    var keyB = -p.price;
    return Comparable.compare(keyA, keyB);
  });
  var skip = 1;
  if (skip < items.length) {
    items = items.sublist(skip);
  } else {
    items = [];
  }
  var take = 3;
  if (take < items.length) {
    items = items.sublist(0, take);
  }
  _res = items;
  return _res;
})();

void main() {
  _structParsers['Product'] = (m) => Product.fromJson(m);
  
  print("--- Top products (excluding most expensive) ---");
  for (var item in expensive) {
    print([item.name.toString(), "costs \\$".toString(), item.price.toString()].join(' '));
  }
}
