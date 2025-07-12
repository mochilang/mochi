import 'dart:convert';

var lineitem = [{'l_quantity': 17, 'l_extendedprice': 1000, 'l_discount': 0.05, 'l_tax': 0.07, 'l_returnflag': 'N', 'l_linestatus': 'O', 'l_shipdate': '1998-08-01'}, {'l_quantity': 36, 'l_extendedprice': 2000, 'l_discount': 0.1, 'l_tax': 0.05, 'l_returnflag': 'N', 'l_linestatus': 'O', 'l_shipdate': '1998-09-01'}, {'l_quantity': 25, 'l_extendedprice': 1500, 'l_discount': 0, 'l_tax': 0.08, 'l_returnflag': 'R', 'l_linestatus': 'F', 'l_shipdate': '1998-09-03'}];

var result = (() {
  var _q0 = <dynamic>[];
  var _g1 = <dynamic, List<dynamic>>{};
  for (var row in lineitem) {
    if (!(row['l_shipdate'].toString().compareTo('1998-09-02') <= 0)) continue;
    var _k16 = {'returnflag': row['l_returnflag'], 'linestatus': row['l_linestatus']};
    _g1.putIfAbsent(_k16, () => <dynamic>[]).add(row);
  }
  for (var entry in _g1.entries) {
    var g = entry.value;
    var _k16 = entry.key;
    _q0.add({'returnflag': _k16['returnflag'], 'linestatus': _k16['linestatus'], 'sum_qty': (() { var _t18 = (() {
  var _q17 = <dynamic>[];
  for (var x in g) {
    _q17.add(x['l_quantity']);
  }
  return _q17;
})(); return _t18.reduce((a, b) => a + b); })(), 'sum_base_price': (() { var _t20 = (() {
  var _q19 = <dynamic>[];
  for (var x in g) {
    _q19.add(x['l_extendedprice']);
  }
  return _q19;
})(); return _t20.reduce((a, b) => a + b); })(), 'sum_disc_price': (() { var _t22 = (() {
  var _q21 = <dynamic>[];
  for (var x in g) {
    _q21.add((x['l_extendedprice'] as num) * ((1 - (x['l_discount'] as num)) as num));
  }
  return _q21;
})(); return _t22.reduce((a, b) => a + b); })(), 'sum_charge': (() { var _t24 = (() {
  var _q23 = <dynamic>[];
  for (var x in g) {
    _q23.add(((x['l_extendedprice'] as num) * ((1 - (x['l_discount'] as num)) as num) as num) * ((1 + (x['l_tax'] as num)) as num));
  }
  return _q23;
})(); return _t24.reduce((a, b) => a + b); })(), 'avg_qty': (() { var _t26 = (() {
  var _q25 = <dynamic>[];
  for (var x in g) {
    _q25.add(x['l_quantity']);
  }
  return _q25;
})(); return (_t26.isEmpty ? 0 : _t26.reduce((a, b) => a + b) / _t26.length); })(), 'avg_price': (() { var _t28 = (() {
  var _q27 = <dynamic>[];
  for (var x in g) {
    _q27.add(x['l_extendedprice']);
  }
  return _q27;
})(); return (_t28.isEmpty ? 0 : _t28.reduce((a, b) => a + b) / _t28.length); })(), 'avg_disc': (() { var _t30 = (() {
  var _q29 = <dynamic>[];
  for (var x in g) {
    _q29.add(x['l_discount']);
  }
  return _q29;
})(); return (_t30.isEmpty ? 0 : _t30.reduce((a, b) => a + b) / _t30.length); })(), 'count_order': g.length});
  }
  return _q0;
})();

void main() {
  print(jsonEncode(result));
}
