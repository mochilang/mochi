var m = {'a': 1, 'b': 2};

void main() {
  var _iter0 = m;
  for (var k in (_iter0 is Map ? (_iter0 as Map).keys : _iter0) as Iterable) {
    print(k);
  }
}
