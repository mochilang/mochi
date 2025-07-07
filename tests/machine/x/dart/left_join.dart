void main() {
  var customers = [
    {'id': 1, 'name': 'Alice'},
    {'id': 2, 'name': 'Bob'}
  ];
  var orders = [
    {'id': 100, 'customerId': 1, 'total': 250},
    {'id': 101, 'customerId': 3, 'total': 80}
  ];

  var result = [];
  for (var o in orders) {
    var c = customers.firstWhere(
      (c) => c['id'] == o['customerId'],
      orElse: () => null,
    );
    result.add({'orderId': o['id'], 'customer': c, 'total': o['total']});
  }

  print('--- Left Join ---');
  for (var entry in result) {
    print('Order ${entry['orderId']} customer ${entry['customer']} total ${entry['total']}');
  }
}
