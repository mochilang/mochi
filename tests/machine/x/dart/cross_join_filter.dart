var nums = [1, 2, 3];

var letters = ['A', 'B'];

var pairs = (() {
  var _q0 = <dynamic>[];
  for (var n in nums) {
    for (var l in letters) {
      if (!((n % 2 as int) == 0)) continue;
      _q0.add({'n': n, 'l': l});
    }
  }
  return _q0;
})();

void main() {
  print('--- Even pairs ---');
  for (var p in pairs) {
    print([p['n'], p['l']].join(' '));
  }
}
