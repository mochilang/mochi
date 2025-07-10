var numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9];

void main() {
  var _iter0 = numbers;
  for (var n in (_iter0 is Map ? (_iter0 as Map).keys : _iter0) as Iterable) {
    if (n % 2 == 0) {
      continue;
    }
    if (n > 7) {
      break;
    }
    print(['odd number:', n].join(' '));
  }
}
