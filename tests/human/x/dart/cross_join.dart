void main() {
  var customers = [
    {'id': 1, 'name': 'Alice'},
    {'id': 2, 'name': 'Bob'},
    {'id': 3, 'name': 'Charlie'}
  ];
  var orders = [
    {'id': 100, 'customerId': 1, 'total': 250},
    {'id': 101, 'customerId': 2, 'total': 125},
    {'id': 102, 'customerId': 1, 'total': 300}
  ];
  print('--- Cross Join: All order-customer pairs ---');
  for (var o in orders) {
    for (var c in customers) {
      print('Order ${o['id']} (customerId: ${o['customerId']}, total: $${o['total']}) paired with ${c['name']}');
    }
  }
}
