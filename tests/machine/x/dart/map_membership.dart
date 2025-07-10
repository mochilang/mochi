bool _in(dynamic item, dynamic col) {
  if (col is Map) return col.containsKey(item);
  if (col is Iterable || col is String) return col.contains(item);
  return false;
}

var m = {'a': 1, 'b': 2};

void main() {
  print(_in('a', m));
  print(_in('c', m));
}
