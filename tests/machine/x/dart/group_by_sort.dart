void main() {
  var items = [{'cat': 'a', 'val': 3}, {'cat': 'a', 'val': 1}, {'cat': 'b', 'val': 5}, {'cat': 'b', 'val': 2}];
  var grouped = (() {
  var _q0 = <dynamic>[];
  for (var i in items) {
    _q0.add([-(() {
  var _q2 = <dynamic>[];
  for (var x in g) {
    _q2.add(x.val);
  }
  return _q2;
})().reduce((a, b) => a + b), {'cat': g.key, 'total': (() {
  var _q1 = <dynamic>[];
  for (var x in g) {
    _q1.add(x.val);
  }
  return _q1;
})().reduce((a, b) => a + b)}]);
  }
  _q0.sort((a,b) => (a[0] as Comparable).compareTo(b[0]));
  _q0 = [for (var x in _q0) x[1]];
  return _q0;
})();
  print(grouped);
}
