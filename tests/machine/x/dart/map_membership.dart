bool _in(dynamic item, dynamic col) {
  if (col is Map) return col.containsKey(item);
  if (col is Iterable || col is String) return col.contains(item);
  return false;
}

void main() {
  var m = {'a': 1, 'b': 2};
  print(_in('a', m));
  print(_in('c', m));
}
