bool _in(dynamic item, dynamic col) {
  if (col is Map) return col.containsKey(item);
  if (col is Iterable || col is String) return col.contains(item);
  return false;
}

var xs = [1, 2, 3];

var ys = (() {
  var _q0 = <dynamic>[];
  for (var x in xs) {
    if (!(x % 2 == 1)) continue;
    _q0.add(x);
  }
  return _q0;
})();

var m = {'a': 1};

var s = 'hello';

void main() {
  print(_in(1, ys));
  print(_in(2, ys));
  print(_in('a', m));
  print(_in('b', m));
  print(_in('ell', s));
  print(_in('foo', s));
}
