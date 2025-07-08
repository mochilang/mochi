void main() {
  var customers = [{'id': 1, 'name': 'Alice'}, {'id': 2, 'name': 'Bob'}];
  var orders = [{'id': 100, 'customerId': 1}, {'id': 101, 'customerId': 1}, {'id': 102, 'customerId': 2}];
  var stats = (() {
  var _q0 = <dynamic>[];
  for (var o in orders) {
    _q0.add({'name': g.key, 'count': g.length});
  }
  return _q0;
})();
  print('--- Orders per customer ---');
  for (var s in stats) {
    print([s.name, 'orders:', s.count].join(' '));
  }
}
