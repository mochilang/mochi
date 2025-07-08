void main() {
  var nations = [{'id': 1, 'name': 'A'}, {'id': 2, 'name': 'B'}];
  var suppliers = [{'id': 1, 'nation': 1}, {'id': 2, 'nation': 2}];
  var partsupp = [{'part': 100, 'supplier': 1, 'cost': 10, 'qty': 2}, {'part': 100, 'supplier': 2, 'cost': 20, 'qty': 1}, {'part': 200, 'supplier': 1, 'cost': 5, 'qty': 3}];
  var filtered = (() {
  var _q0 = <dynamic>[];
  for (var ps in partsupp) {
    if (!(n.name == 'A')) continue;
    _q0.add({'part': ps.part, 'value': ps.cost * ps.qty});
  }
  return _q0;
})();
  var grouped = (() {
  var _q1 = <dynamic>[];
  for (var x in filtered) {
    _q1.add({'part': g.key, 'total': (() {
  var _q2 = <dynamic>[];
  for (var r in g) {
    _q2.add(r.value);
  }
  return _q2;
})().reduce((a, b) => a + b)});
  }
  return _q1;
})();
  print(grouped);
}
