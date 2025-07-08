void main() {
  var items = [{'cat': 'a', 'val': 10, 'flag': true}, {'cat': 'a', 'val': 5, 'flag': false}, {'cat': 'b', 'val': 20, 'flag': true}];
  var result = (() {
  var _q0 = <dynamic>[];
  for (var i in items) {
    _q0.add([g.key, {'cat': g.key, 'share': (() {
  var _q1 = <dynamic>[];
  for (var x in g) {
    _q1.add((x.flag ? x.val : 0));
  }
  return _q1;
})().reduce((a, b) => a + b) / (() {
  var _q2 = <dynamic>[];
  for (var x in g) {
    _q2.add(x.val);
  }
  return _q2;
})().reduce((a, b) => a + b)}]);
  }
  _q0.sort((a,b) => (a[0] as Comparable).compareTo(b[0]));
  _q0 = [for (var x in _q0) x[1]];
  return _q0;
})();
  print(result);
}
