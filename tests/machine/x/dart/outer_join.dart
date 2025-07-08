void main() {
  var customers = [{'id': 1, 'name': 'Alice'}, {'id': 2, 'name': 'Bob'}, {'id': 3, 'name': 'Charlie'}, {'id': 4, 'name': 'Diana'}];
  var orders = [{'id': 100, 'customerId': 1, 'total': 250}, {'id': 101, 'customerId': 2, 'total': 125}, {'id': 102, 'customerId': 1, 'total': 300}, {'id': 103, 'customerId': 5, 'total': 80}];
  var result = (() {
  var _q0 = <dynamic>[];
  for (var o in orders) {
    _q0.add({'order': o, 'customer': c});
  }
  return _q0;
})();
  print('--- Outer Join using syntax ---');
  for (var row in result) {
    if (row.order) {
      if (row.customer) {
        print(['Order', row.order.id, 'by', row.customer.name, '- $', row.order.total].join(' '));
      }
      else {
        print(['Order', row.order.id, 'by', 'Unknown', '- $', row.order.total].join(' '));
      }
    }
    else {
      print(['Customer', row.customer.name, 'has no orders'].join(' '));
    }
  }
}
