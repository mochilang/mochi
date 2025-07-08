bool _in(dynamic item, dynamic col) {
  if (col is Map) return col.containsKey(item);
  if (col is Iterable || col is String) return col.contains(item);
  return false;
}

void main() {
  var m = {1: 'a', 2: 'b'};
  print(_in(1, m));
  print(_in(3, m));
}
