void main() {
  var customers = [{'id': 1, 'name': 'Alice'}, {'id': 2, 'name': 'Bob'}, {'id': 3, 'name': 'Charlie'}];
  var orders = [{'id': 100, 'customerId': 1}, {'id': 101, 'customerId': 1}, {'id': 102, 'customerId': 2}];
  var stats = (() {
  var _q0 = <dynamic>[];
  for (var c in customers) {
    _q0.add({'name': g.key, 'count': (() {
  var _q1 = <dynamic>[];
  for (var r in g) {
    if (!(r.o)) continue;
    _q1.add(r);
  }
  return _q1;
})().length});
  }
  return _q0;
})();
  print('--- Group Left Join ---');
  for (var s in stats) {
    print([s.name, 'orders:', s.count].join(' '));
  }
}
