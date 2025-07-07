void main() {
  var products = [
    {'name': 'Laptop', 'price': 1500},
    {'name': 'Smartphone', 'price': 900},
    {'name': 'Tablet', 'price': 600},
    {'name': 'Monitor', 'price': 300},
    {'name': 'Keyboard', 'price': 100},
    {'name': 'Mouse', 'price': 50},
    {'name': 'Headphones', 'price': 200}
  ];
  var expensive = List.from(products)
    ..sort((a, b) => b['price'].compareTo(a['price']))
    ..removeAt(0);
  expensive = expensive.take(3).toList();
  print('--- Top products (excluding most expensive) ---');
  for (var item in expensive) {
    print('${item['name']} costs $${item['price']}');
  }
}
