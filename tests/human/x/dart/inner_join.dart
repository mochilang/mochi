void main() {
  var customers = [
    {'id': 1, 'name': 'Alice'},
    {'id': 2, 'name': 'Bob'},
    {'id': 3, 'name': 'Charlie'}
  ];
  var orders = [
    {'id': 100, 'customerId': 1, 'total': 250},
    {'id': 101, 'customerId': 2, 'total': 125},
    {'id': 102, 'customerId': 1, 'total': 300},
    {'id': 103, 'customerId': 4, 'total': 80}
  ];

  var result = [];
  for (var o in orders) {
    var c = customers.firstWhere(
      (c) => c['id'] == o['customerId'],
      orElse: () => null,
    );
    if (c != null) {
      result.add({'orderId': o['id'], 'customerName': c['name'], 'total': o['total']});
    }
  }

  print('--- Orders with customer info ---');
  for (var entry in result) {
    print('Order ${entry['orderId']} by ${entry['customerName']} - \$${entry['total']}');
  }
}
