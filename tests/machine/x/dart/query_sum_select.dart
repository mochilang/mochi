// Generated by Mochi compiler v0.10.27 on 2025-07-17T11:53:04Z
var nums = [1, 2, 3];

var result = (() {
  var _q0 = <dynamic>[];
  for (var n in nums) {
    if (!(n > 1)) continue;
    _q0.add(n);
  }
  return _sum(_q0);
})();

void main() {
  print(result);
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
