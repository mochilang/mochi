var nums = [1, 2, 3];

var letters = ['A', 'B'];

var pairs = (() {
  var _q0 = <dynamic>[];
  for (var n in nums) {
    for (var l in letters) {
      if (!(n % 2 == 0)) continue;
      _q0.add({'n': n, 'l': l});
    }
  }
  return _q0;
})();

void main() {
  print('--- Even pairs ---');
  var _iter1 = pairs;
  for (var p in (_iter1 is Map ? (_iter1 as Map).keys : _iter1) as Iterable) {
    print([p['n'], p['l']].join(' '));
  }
}
