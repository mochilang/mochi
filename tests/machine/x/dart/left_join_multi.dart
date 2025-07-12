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
    for (var c in customers) {
      if (!(o['customerId'] == c['id'])) continue;
      var _jt1 = <dynamic>[];
      for (var i in items) {
        if (!(o['id'] == i['orderId'])) continue;
        _jt1.add(i);
      }
      if (_jt1.isEmpty) _jt1.add(null);
      for (var i in _jt1) {
        _q0.add({'orderId': o['id'], 'name': c['name'], 'item': i});
      }
    }
  }
  return _q0;
})();

void main() {
  print('--- Left Join Multi ---');
  for (var r in result) {
    print([r['orderId'], r['name'], r['item']].join(' '));
  }
}
