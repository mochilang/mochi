void main() {
  var nation = [{'n_nationkey': 1, 'n_name': 'BRAZIL'}];
  var customer = [{'c_custkey': 1, 'c_name': 'Alice', 'c_acctbal': 100, 'c_nationkey': 1, 'c_address': '123 St', 'c_phone': '123-456', 'c_comment': 'Loyal'}];
  var orders = [{'o_orderkey': 1000, 'o_custkey': 1, 'o_orderdate': '1993-10-15'}, {'o_orderkey': 2000, 'o_custkey': 1, 'o_orderdate': '1994-01-02'}];
  var lineitem = [{'l_orderkey': 1000, 'l_returnflag': 'R', 'l_extendedprice': 1000, 'l_discount': 0.1}, {'l_orderkey': 2000, 'l_returnflag': 'N', 'l_extendedprice': 500, 'l_discount': 0}];
  var start_date = '1993-10-01';
  var end_date = '1994-01-01';
  var result = (() {
  var _q0 = <dynamic>[];
  for (var c in customer) {
    if (!(o.o_orderdate >= start_date && o.o_orderdate < end_date && l.l_returnflag == 'R')) continue;
    _q0.add([-(() {
  var _q2 = <dynamic>[];
  for (var x in g) {
    _q2.add(x.l.l_extendedprice * (1 - x.l.l_discount));
  }
  return _q2;
})().reduce((a, b) => a + b), {'c_custkey': g.key.c_custkey, 'c_name': g.key.c_name, 'revenue': (() {
  var _q1 = <dynamic>[];
  for (var x in g) {
    _q1.add(x.l.l_extendedprice * (1 - x.l.l_discount));
  }
  return _q1;
})().reduce((a, b) => a + b), 'c_acctbal': g.key.c_acctbal, 'n_name': g.key.n_name, 'c_address': g.key.c_address, 'c_phone': g.key.c_phone, 'c_comment': g.key.c_comment}]);
  }
  _q0.sort((a,b) => (a[0] as Comparable).compareTo(b[0]));
  _q0 = [for (var x in _q0) x[1]];
  return _q0;
})();
  print(result);
}
