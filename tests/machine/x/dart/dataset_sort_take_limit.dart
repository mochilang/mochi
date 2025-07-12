import 'dart:convert';

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
  _q0.sort(
    (a, b) => (jsonEncode(a[0]) as Comparable).compareTo(jsonEncode(b[0])),
  );
  _q0 = [for (var x in _q0) x[1]];
  _q0 = _q0.sublist(1, (1) + (3));
  return _q0;
})();

void main() {
  print('--- Top products (excluding most expensive) ---');
  for (var item in expensive) {
    print([item['name'], 'costs \$', item['price']].join(' '));
  }
}
