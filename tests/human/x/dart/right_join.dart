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
    {'id': 102, 'customerId': 1, 'total': 300}
  ];

  var result = [];
  for (var c in customers) {
    var matched = orders.where((o) => o['customerId'] == c['id']).toList();
    if (matched.isEmpty) {
      result.add({'customerName': c['name'], 'order': null});
    } else {
      for (var o in matched) {
        result.add({'customerName': c['name'], 'order': o});
      }
    }
  }

  print('--- Right Join using syntax ---');
  for (var entry in result) {
    if (entry['order'] != null) {
      print('Customer ${entry['customerName']} has order ${entry['order']['id']} - \$${entry['order']['total']}');
    } else {
      print('Customer ${entry['customerName']} has no orders');
    }
  }
}
