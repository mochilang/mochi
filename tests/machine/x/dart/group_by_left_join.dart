var customers = [{'id': 1, 'name': 'Alice'}, {'id': 2, 'name': 'Bob'}, {'id': 3, 'name': 'Charlie'}];

var orders = [{'id': 100, 'customerId': 1}, {'id': 101, 'customerId': 1}, {'id': 102, 'customerId': 2}];

var stats = (() {
  var _q0 = <dynamic>[];
  var _g1 = <dynamic, List<dynamic>>{};
  for (var c in customers) {
    var _jt2 = <dynamic>[];
    for (var o in orders) {
      if (!(o['customerId'] == c['id'])) continue;
      _jt2.add(o);
    }
    if (_jt2.isEmpty) _jt2.add(null);
    for (var o in _jt2) {
      var _k4 = c['name'];
      _g1.putIfAbsent(_k4, () => <dynamic>[]).add(c);
    }
  }
  for (var entry in _g1.entries) {
    var g = entry.value;
    var _k4 = entry.key;
    _q0.add({'name': _k4, 'count': (() {
  var _q5 = <dynamic>[];
  for (var r in g) {
    if (!(r['o'])) continue;
    _q5.add(r);
  }
  return _q5;
})().length});
  }
  return _q0;
})();

void main() {
  print('--- Group Left Join ---');
  var _iter6 = stats;
  for (var s in (_iter6 is Map ? (_iter6 as Map).keys : _iter6) as Iterable) {
    print([s['name'], 'orders:', s['count']].join(' '));
  }
}

