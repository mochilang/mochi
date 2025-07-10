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
  {'id': 103, 'customerId': 5, 'total': 80},
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
      _q0.add({'order': o, 'customer': c});
    }
  }
  for (var c in customers) {
    var _f = false;
    for (var o in orders) {
      if (!(o['customerId'] == c['id'])) continue;
      _f = true;
      break;
    }
    if (!_f) {
      var o = null;
      _q0.add({'order': o, 'customer': c});
    }
  }
  return _q0;
})();

void main() {
  print('--- Outer Join using syntax ---');
  var _iter2 = result;
  for (var row in (_iter2 is Map ? (_iter2 as Map).keys : _iter2) as Iterable) {
    if (row['order']) {
      if (row['customer']) {
        print(
          [
            'Order',
            row['order']['id'],
            'by',
            row['customer']['name'],
            '- \$',
            row['order']['total'],
          ].join(' '),
        );
      } else {
        print(
          [
            'Order',
            row['order']['id'],
            'by',
            'Unknown',
            '- \$',
            row['order']['total'],
          ].join(' '),
        );
      }
    } else {
      print(['Customer', row['customer']['name'], 'has no orders'].join(' '));
    }
  }
}
