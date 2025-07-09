void main() {
  var customers = [
    {'id': 1, 'name': 'Alice'},
    {'id': 2, 'name': 'Bob'},
  ];
  var orders = [
    {'id': 100, 'customerId': 1},
    {'id': 101, 'customerId': 1},
    {'id': 102, 'customerId': 2},
  ];
  var stats = (() {
    var _q0 = <dynamic>[];
    var _g1 = <dynamic, List<dynamic>>{};
    for (var o in orders) {
      var _k2 = c.name;
      _g1.putIfAbsent(_k2, () => <dynamic>[]).add(o);
    }
    for (var entry in _g1.entries) {
      var g = entry.value;
      var _k2 = entry.key;
      _q0.add({'name': _k2, 'count': g.length});
    }
    return _q0;
  })();
  print('--- Orders per customer ---');
  var _iter3 = stats;
  for (var s in (_iter3 is Map ? (_iter3 as Map).keys : _iter3) as Iterable) {
    print([s['name'], 'orders:', s['count']].join(' '));
  }
}
