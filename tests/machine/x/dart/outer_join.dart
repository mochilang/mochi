void main() {
  var customers = [
    {'id': 1, 'name': 'Alice'},
    {'id': 2, 'name': 'Bob'},
    {'id': 3, 'name': 'Charlie'},
    {'id': 4, 'name': 'Diana'}
  ];

  var orders = [
    {'id': 100, 'customerId': 1, 'total': 250},
    {'id': 101, 'customerId': 2, 'total': 125},
    {'id': 102, 'customerId': 1, 'total': 300},
    {'id': 103, 'customerId': 5, 'total': 80}
  ];

  var result = [];
  for (var o in orders) {
    var c = customers.firstWhere((c) => c['id'] == o['customerId'], orElse: () => null);
    result.add({'order': o, 'customer': c});
  }
  for (var c in customers) {
    var has = orders.any((o) => o['customerId'] == c['id']);
    if (!has) result.add({'order': null, 'customer': c});
  }

  print('--- Outer Join using syntax ---');
  for (var row in result) {
    if (row['order'] != null) {
      if (row['customer'] != null) {
        print('Order ${row['order']['id']} by ${row['customer']['name']} - \$${row['order']['total']}');
      } else {
        print('Order ${row['order']['id']} by Unknown - \$${row['order']['total']}');
      }
    } else {
      print('Customer ${row['customer']['name']} has no orders');
    }
  }
}
