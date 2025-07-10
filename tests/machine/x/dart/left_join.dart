var customers = [
  {'id': 1, 'name': 'Alice'},
  {'id': 2, 'name': 'Bob'},
];

var orders = [
  {'id': 100, 'customerId': 1, 'total': 250},
  {'id': 101, 'customerId': 3, 'total': 80},
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
      _q0.add({'orderId': o['id'], 'customer': c, 'total': o['total']});
    }
  }
  return _q0;
})();

void main() {
  print('--- Left Join ---');
  var _iter2 = result;
  for (var entry
      in (_iter2 is Map ? (_iter2 as Map).keys : _iter2) as Iterable) {
    print(
      [
        'Order',
        entry['orderId'],
        'customer',
        entry['customer'],
        'total',
        entry['total'],
      ].join(' '),
    );
  }
}
