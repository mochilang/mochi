void main() {
  var customers = [
    {'id': 1, 'name': 'Alice'},
    {'id': 2, 'name': 'Bob'},
    {'id': 3, 'name': 'Charlie'},
  ];
  var orders = [
    {'id': 100, 'customerId': 1, 'total': 250},
    {'id': 101, 'customerId': 2, 'total': 125},
    {'id': 102, 'customerId': 1, 'total': 300},
  ];
  var result = (() {
    var _q0 = <dynamic>[];
    for (var o in orders) {
      for (var c in customers) {
        _q0.add({
          'orderId': o['id'],
          'orderCustomerId': o['customerId'],
          'pairedCustomerName': c['name'],
          'orderTotal': o['total'],
        });
      }
    }
    return _q0;
  })();
  print('--- Cross Join: All order-customer pairs ---');
  var _iter1 = result;
  for (var entry
      in (_iter1 is Map ? (_iter1 as Map).keys : _iter1) as Iterable) {
    print(
      [
        'Order',
        entry['orderId'],
        '(customerId:',
        entry['orderCustomerId'],
        ', total: \$',
        entry['orderTotal'],
        ') paired with',
        entry['pairedCustomerName'],
      ].join(' '),
    );
  }
}
