void main() {
  var customers = [{'id': 1, 'name': 'Alice'}, {'id': 2, 'name': 'Bob'}];
  var orders = [{'id': 100, 'customerId': 1, 'total': 250}, {'id': 101, 'customerId': 3, 'total': 80}];
  var result = (() {
  var _q0 = <dynamic>[];
  for (var o in orders) {
    _q0.add({'orderId': o.id, 'customer': c, 'total': o.total});
  }
  return _q0;
})();
  print('--- Left Join ---');
  for (var entry in result) {
    print(['Order', entry.orderId, 'customer', entry.customer, 'total', entry.total].join(' '));
  }
}
