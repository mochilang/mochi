void main() {
  var customers = [
    {'id': 1, 'name': 'Alice'},
    {'id': 2, 'name': 'Bob'},
    {'id': 3, 'name': 'Charlie'},
  ];
  var orders = [
    {'id': 100, 'customerId': 1},
    {'id': 101, 'customerId': 1},
    {'id': 102, 'customerId': 2},
  ];
  var stats = (() {
    var _q0 = <dynamic>[];
    var _g1 = <dynamic, List<dynamic>>{};
    for (var c in customers) {
      var _k3 = c['name'];
      _g1.putIfAbsent(_k3, () => <dynamic>[]).add(c);
    }
    for (var entry in _g1.entries) {
      var g = entry.value;
      var _k3 = entry.key;
      _q0.add({
        'name': _k3,
        'count': (() {
          var _q4 = <dynamic>[];
          for (var r in g) {
            if (!(r['o'])) continue;
            _q4.add(r);
          }
          return _q4;
        })().length,
      });
    }
    return _q0;
  })();
  print('--- Group Left Join ---');
  var _iter5 = stats;
  for (var s in (_iter5 is Map ? (_iter5 as Map).keys : _iter5) as Iterable) {
    print([s['name'], 'orders:', s['count']].join(' '));
  }
}
