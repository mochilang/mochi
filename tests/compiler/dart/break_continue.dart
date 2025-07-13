List<int> numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9];

void main() {
  for (var n in numbers) {
    if (_equal((n % 2), 0)) {
      continue;
    }
    if ((n > 7)) {
      break;
    }
    print(["odd number:".toString(), n.toString()].join(' '));
  }
}

bool _equal(dynamic a, dynamic b) {
    if (a is List && b is List) {
        if (a.length != b.length) return false;
        for (var i = 0; i < a.length; i++) { if (!_equal(a[i], b[i])) return false; }
        return true;
    }
    if (a is Map && b is Map) {
        if (a.length != b.length) return false;
        for (var k in a.keys) { if (!b.containsKey(k) || !_equal(a[k], b[k])) return false; }
        return true;
    }
    return a == b;
}
