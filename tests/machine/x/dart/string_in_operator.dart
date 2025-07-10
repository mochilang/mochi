bool _in(dynamic item, dynamic col) {
  if (col is Map) return col.containsKey(item);
  if (col is Iterable || col is String) return col.contains(item);
  return false;
}

var s = 'catch';

void main() {
  print(_in('cat', s));
  print(_in('dog', s));
}
