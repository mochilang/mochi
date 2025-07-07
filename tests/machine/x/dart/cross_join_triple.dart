void main() {
  var nums = [1, 2];
  var letters = ['A', 'B'];
  var bools = [true, false];
  var combos = <Map<String, dynamic>>[];
  for (var n in nums) {
    for (var l in letters) {
      for (var b in bools) {
        combos.add({'n': n, 'l': l, 'b': b});
      }
    }
  }
  print('--- Cross Join of three lists ---');
  for (var c in combos) {
    print('${c['n']} ${c['l']} ${c['b']}');
  }
}
