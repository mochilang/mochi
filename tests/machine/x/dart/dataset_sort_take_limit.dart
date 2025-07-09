void main() {
  var products = [
    {'name': 'Laptop', 'price': 1500},
    {'name': 'Smartphone', 'price': 900},
    {'name': 'Tablet', 'price': 600},
    {'name': 'Monitor', 'price': 300},
    {'name': 'Keyboard', 'price': 100},
    {'name': 'Mouse', 'price': 50},
    {'name': 'Headphones', 'price': 200},
  ];
  var expensive = (() {
    var _q0 = <dynamic>[];
    for (var p in products) {
      _q0.add([-(p['price'] as num), p]);
    }
    _q0.sort((a, b) => (a[0] as Comparable).compareTo(b[0]));
    _q0 = [for (var x in _q0) x[1]];
    _q0 = _q0.sublist(1, (1) + (3));
    return _q0;
  })();
  print('--- Top products (excluding most expensive) ---');
  var _iter1 = expensive;
  for (var item
      in (_iter1 is Map ? (_iter1 as Map).keys : _iter1) as Iterable) {
    print([item['name'], 'costs \$', item['price']].join(' '));
  }
}
