void main() {
  var customers = [
    {'id': 1, 'name': 'Alice'},
    {'id': 2, 'name': 'Bob'},
  ];
  var orders = [
    {'id': 100, 'customerId': 1},
    {'id': 101, 'customerId': 2},
  ];
  var items = [
    {'orderId': 100, 'sku': 'a'},
  ];
  var result = (() {
    var _q0 = <dynamic>[];
    for (var o in orders) {
      _q0.add({'orderId': o['id'], 'name': c.name, 'item': i});
    }
    return _q0;
  })();
  print('--- Left Join Multi ---');
  var _iter1 = result;
  for (var r in (_iter1 is Map ? (_iter1 as Map).keys : _iter1) as Iterable) {
    print([r['orderId'], r['name'], r['item']].join(' '));
  }
}
