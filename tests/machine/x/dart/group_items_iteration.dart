void main() {
  var data = [{'tag': 'a', 'val': 1}, {'tag': 'a', 'val': 2}, {'tag': 'b', 'val': 3}];
  var groups = (() {
  var _q0 = <dynamic>[];
  for (var d in data) {
    _q0.add(g);
  }
  return _q0;
})();
  var tmp = [];
  for (var g in groups) {
    var total = 0;
    for (var x in g.items) {
      total = total + x.val;
    }
    tmp = List.from(tmp)..add({'tag': g.key, 'total': total});
  }
  var result = (() {
  var _q1 = <dynamic>[];
  for (var r in tmp) {
    _q1.add([r.tag, r]);
  }
  _q1.sort((a,b) => (a[0] as Comparable).compareTo(b[0]));
  _q1 = [for (var x in _q1) x[1]];
  return _q1;
})();
  print(result);
}
