// Generated by Mochi compiler v0.10.28 on 2025-07-18T03:32:16Z
import 'dart:io';
import 'dart:convert';

var customer = [
  {
  'c_custkey': 1,
  'c_mktsegment': 'BUILDING',
},
  {
  'c_custkey': 2,
  'c_mktsegment': 'AUTOMOBILE',
},
];

var orders = [
  {
  'o_orderkey': 100,
  'o_custkey': 1,
  'o_orderdate': '1995-03-14',
  'o_shippriority': 1,
},
  {
  'o_orderkey': 200,
  'o_custkey': 2,
  'o_orderdate': '1995-03-10',
  'o_shippriority': 2,
},
];

var lineitem = [
  {
  'l_orderkey': 100,
  'l_extendedprice': 1000,
  'l_discount': 0.05,
  'l_shipdate': '1995-03-16',
},
  {
  'l_orderkey': 100,
  'l_extendedprice': 500,
  'l_discount': 0,
  'l_shipdate': '1995-03-20',
},
  {
  'l_orderkey': 200,
  'l_extendedprice': 1000,
  'l_discount': 0.1,
  'l_shipdate': '1995-03-14',
},
];

var cutoff = '1995-03-15';

var segment = 'BUILDING';

var building_customers = (() {
  var _q0 = <dynamic>[];
  for (var c in customer) {
    if (!(c['c_mktsegment'] == segment)) continue;
    _q0.add(c);
  }
  return _q0;
})();

var valid_orders = (() {
  var _q1 = <dynamic>[];
  for (var o in orders) {
    for (var c in building_customers) {
      if (!(o['o_custkey'] == c['c_custkey'])) continue;
      if (!(o['o_orderdate'].toString().compareTo(cutoff.toString()) < 0)) continue;
      _q1.add(o);
    }
  }
  return _q1;
})();

var valid_lineitems = (() {
  var _q2 = <dynamic>[];
  for (var l in lineitem) {
    if (!(l['l_shipdate'].toString().compareTo(cutoff.toString()) > 0)) continue;
    _q2.add(l);
  }
  return _q2;
})();

var order_line_join = (() {
  var _q3 = <dynamic>[];
  var _g4 = <String, List<dynamic>>{};
  for (var o in valid_orders) {
    for (var l in valid_lineitems) {
      if (!(l['l_orderkey'] == o['o_orderkey'])) continue;
      var _k6 = {
  'o_orderkey': o['o_orderkey'],
  'o_orderdate': o['o_orderdate'],
  'o_shippriority': o['o_shippriority'],
};
      var _k6_s = jsonEncode(_k6);
      _g4.putIfAbsent(_k6_s, () => <dynamic>[]).add({'o': o, 'l': l});
    }
  }
  for (var entry in _g4.entries) {
    var g = entry.value;
    var _k6 = jsonDecode(entry.key);
    _q3.add([[
  -(_sum(g.map((r) => (r['l']['l_extendedprice'] as num) * ((1 - (r['l']['l_discount'] as num)) as num))) as num),
  _k6['o_orderdate'],
], {
  'l_orderkey': _k6['o_orderkey'],
  'revenue': _sum(g.map((r) => (r['l']['l_extendedprice'] as num) * ((1 - (r['l']['l_discount'] as num)) as num))),
  'o_orderdate': _k6['o_orderdate'],
  'o_shippriority': _k6['o_shippriority'],
}]);
  }
  _q3.sort((a,b) => (jsonEncode(a[0]) as Comparable).compareTo(jsonEncode(b[0])));
  _q3 = [for (var x in _q3) x[1]];
  return _q3;
})();

void main() {
  print(jsonEncode(order_line_join));
}

dynamic _min(dynamic v) {
    List<dynamic>? list;
    if (v is List) list = v;
    else if (v is Map && v['items'] is List) list = (v['items'] as List);
    else if (v is Map && v['Items'] is List) list = (v['Items'] as List);
    else { try { var it = (v as dynamic).items; if (it is List) list = it; } catch (_) {} }
    if (list == null || list.isEmpty) return 0;
    var m = list[0];
    for (var n in list) { if ((n as Comparable).compareTo(m) < 0) m = n; }
    return m;
}

num _sum(dynamic v) {
    Iterable<dynamic>? list;
    if (v is Iterable) list = v;
    else if (v is Map && v['items'] is Iterable) list = (v['items'] as Iterable);
    else if (v is Map && v['Items'] is Iterable) list = (v['Items'] as Iterable);
    else { try { var it = (v as dynamic).items; if (it is Iterable) list = it; } catch (_) {} }
    if (list == null) return 0;
    num s = 0;
    for (var n in list) s += (n as num);
    return s;
}
