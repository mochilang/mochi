void main() {
  var nums = [1, 2];
  var letters = ['A', 'B'];
  var bools = [true, false];
  var combos = (() {
  var _q0 = <dynamic>[];
  for (var n in nums) {
    for (var l in letters) {
      for (var b in bools) {
        _q0.add({'n': n, 'l': l, 'b': b});
      }
    }
  }
  return _q0;
})();
  print('--- Cross Join of three lists ---');
  for (var c in combos) {
    print([c.n, c.l, c.b].join(' '));
  }
}
