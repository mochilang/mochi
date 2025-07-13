import 'dart:io';
import 'dart:convert';

List<Map<String, dynamic>> lineitem = [{"l_quantity": 17, "l_extendedprice": 1000, "l_discount": 0.05, "l_tax": 0.07, "l_returnflag": "N", "l_linestatus": "O", "l_shipdate": "1998-08-01"}, {"l_quantity": 36, "l_extendedprice": 2000, "l_discount": 0.1, "l_tax": 0.05, "l_returnflag": "N", "l_linestatus": "O", "l_shipdate": "1998-09-01"}, {"l_quantity": 25, "l_extendedprice": 1500, "l_discount": 0, "l_tax": 0.08, "l_returnflag": "R", "l_linestatus": "F", "l_shipdate": "1998-09-03"}];

List<Map<String, dynamic>> result = (() {
  var groups = <String,_Group>{};
  var order = <String>[];
  for (var row in lineitem) {
    var key = {"returnflag": row['l_returnflag'], "linestatus": row['l_linestatus']};
    var ks = key.toString();
    var g = groups[ks];
    if (g == null) {
      g = _Group(key);
      groups[ks] = g;
      order.add(ks);
    }
    g.Items.add(row);
  }
  var items = [for (var k in order) groups[k]!];
  var _res = [];
  for (var g in items) {
    _res.add({"returnflag": g.key.returnflag, "linestatus": g.key.linestatus, "sum_qty": _sum((() {
  var _res = [];
  for (var x in g) {
    _res.add(x.l_quantity);
  }
  return _res;
})()), "sum_base_price": _sum((() {
  var _res = [];
  for (var x in g) {
    _res.add(x.l_extendedprice);
  }
  return _res;
})()), "sum_disc_price": _sum((() {
  var _res = [];
  for (var x in g) {
    _res.add((x.l_extendedprice * ((1 - x.l_discount))));
  }
  return _res;
})()), "sum_charge": _sum((() {
  var _res = [];
  for (var x in g) {
    _res.add(((x.l_extendedprice * ((1 - x.l_discount))) * ((1 + x.l_tax))));
  }
  return _res;
})()), "avg_qty": _avg((() {
  var _res = [];
  for (var x in g) {
    _res.add(x.l_quantity);
  }
  return _res;
})()), "avg_price": _avg((() {
  var _res = [];
  for (var x in g) {
    _res.add(x.l_extendedprice);
  }
  return _res;
})()), "avg_disc": _avg((() {
  var _res = [];
  for (var x in g) {
    _res.add(x.l_discount);
  }
  return _res;
})()), "count_order": _count(g)});
  }
  return _res;
})();

void test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus() {
  if (!(_equal(result, [{"returnflag": "N", "linestatus": "O", "sum_qty": 53, "sum_base_price": 3000, "sum_disc_price": (950 + 1800), "sum_charge": (((950 * 1.07)) + ((1800 * 1.05))), "avg_qty": 26.5, "avg_price": 1500, "avg_disc": 0.07500000000000001, "count_order": 2}]))) { throw Exception('expect failed'); }
}

void main() {
  int failures = 0;
  _json(result);
  if (!_runTest("Q1 aggregates revenue and quantity by returnflag + linestatus", test_Q1_aggregates_revenue_and_quantity_by_returnflag___linestatus)) failures++;
  if (failures > 0) {
    print("\n[FAIL] $failures test(s) failed.");
  }
}

class _Group {
    dynamic key;
    List<dynamic> Items = [];
    _Group(this.key);
    int count() => _count(this);
    double sum() => _sum(this);
    double avg() => _avg(this);
}

double _avg(dynamic v) {
    List<dynamic>? list;
    if (v is List) list = v;
    else if (v is Map && v['items'] is List) list = (v['items'] as List);
    else if (v is Map && v['Items'] is List) list = (v['Items'] as List);
    else if (v is _Group) list = v.Items;
    else { try { var it = (v as dynamic).items; if (it is List) list = it; } catch (_) {} }
    if (list == null || list.isEmpty) return 0;
    var s = 0.0;
    for (var n in list) s += (n as num).toDouble();
    return s / list.length;
}

int _count(dynamic v) {
    if (v is String) return v.runes.length;
    if (v is List) return v.length;
    if (v is Map) return v.length;
    try { var items = (v as dynamic).Items; if (items is List) return items.length; } catch (_) {}
    try { var items = (v as dynamic).items; if (items is List) return items.length; } catch (_) {}
    return 0;
}

bool _equal(dynamic a, dynamic b) {
    if (a is List && b is List) {
        if (a.length != b.length) return false;
        for (var i = 0; i < a.length; i++) { if (!_equal(a[i], b[i])) return false; }
        return true;
    }
    if (a is Map && b is Map) {
        if (a.length != b.length) return false;
        for (var k in a.keys) { if (!b.containsKey(k) || !_equal(a[k], b[k])) return false; }
        return true;
    }
    return a == b;
}

String _formatDuration(Duration d) {
    if (d.inMicroseconds < 1000) return '${d.inMicroseconds}µs';
    if (d.inMilliseconds < 1000) return '${d.inMilliseconds}ms';
    return '${(d.inMilliseconds/1000).toStringAsFixed(2)}s';
}

void _json(dynamic v) {
    print(jsonEncode(v));
}

bool _runTest(String name, void Function() f) {
    stdout.write('   test $name ...');
    var start = DateTime.now();
    try {
        f();
        var d = DateTime.now().difference(start);
        stdout.writeln(' ok (${_formatDuration(d)})');
        return true;
    } catch (e) {
        var d = DateTime.now().difference(start);
        stdout.writeln(' fail $e (${_formatDuration(d)})');
        return false;
    }
}

double _sum(dynamic v) {
    List<dynamic>? list;
    if (v is List) list = v;
    else if (v is Map && v['items'] is List) list = (v['items'] as List);
    else if (v is Map && v['Items'] is List) list = (v['Items'] as List);
    else if (v is _Group) list = v.Items;
    else { try { var it = (v as dynamic).items; if (it is List) list = it; } catch (_) {} }
    if (list == null || list.isEmpty) return 0;
    var s = 0.0;
    for (var n in list) s += (n as num).toDouble();
    return s;
}
