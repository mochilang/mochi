bool _in(dynamic item, dynamic col) {
  if (col is Map) return col.containsKey(item);
  if (col is Iterable || col is String) return col.contains(item);
  return false;
}

void main() {
  var xs = [1, 2, 3];
  var ys = (() {
    var _q0 = <dynamic>[];
    for (var x in xs) {
      if (!(x % 2 == 1)) continue;
      _q0.add(x);
    }
    return _q0;
  })();
  print(_in(1, ys));
  print(_in(2, ys));
  var m = {'a': 1};
  print(_in('a', m));
  print(_in('b', m));
  var s = 'hello';
  print(_in('ell', s));
  print(_in('foo', s));
}
