bool _in(dynamic item, dynamic col) {
  if (col is Map) return col.containsKey(item);
  if (col is Iterable || col is String) return col.contains(item);
  return false;
}

void main() {
  var xs = [1, 2, 3];
  print(_in(2, xs));
  print(!(_in(5, xs)));
}
