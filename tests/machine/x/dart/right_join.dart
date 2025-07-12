var customers = [
  {'id': 1, 'name': 'Alice'},
  {'id': 2, 'name': 'Bob'},
  {'id': 3, 'name': 'Charlie'},
  {'id': 4, 'name': 'Diana'},
];

var orders = [
  {'id': 100, 'customerId': 1, 'total': 250},
  {'id': 101, 'customerId': 2, 'total': 125},
  {'id': 102, 'customerId': 1, 'total': 300},
];

var result = (() {
  var _q0 = <dynamic>[];
  for (var o in orders) {
    var _jt1 = <dynamic>[];
    for (var c in customers) {
      if (!(o['customerId'] == c['id'])) continue;
      _jt1.add(c);
    }
    if (_jt1.isEmpty) _jt1.add(null);
    for (var c in _jt1) {
      _q0.add({'customerName': c['name'], 'order': o});
    }
  }
  return _q0;
})();

void main() {
  print('--- Right Join using syntax ---');
  for (var entry in result) {
    if (entry['order'] != null) {
      print(
        [
          'Customer',
          entry['customerName'],
          'has order',
          entry['order']['id'],
          '- \$',
          entry['order']['total'],
        ].join(' '),
      );
    } else {
      print(['Customer', entry['customerName'], 'has no orders'].join(' '));
    }
  }
}
