bool _in(dynamic item, dynamic col) {
  if (col is Map) return col.containsKey(item);
  if (col is Iterable || col is String) return col.contains(item);
  return false;
}

var m = {1: 'a', 2: 'b'};

void main() {
  print(_in(1, m));
  print(_in(3, m));
}
