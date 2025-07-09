void main() {
  var _iter0 = [1, 2, 3];
  for (var n in (_iter0 is Map ? (_iter0 as Map).keys : _iter0) as Iterable) {
    print(n);
  }
}
