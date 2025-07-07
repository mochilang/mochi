void main() {
  var nums = [1, 2, 3];
  var letters = ['A', 'B'];
  var pairs = <Map<String, dynamic>>[];
  for (var n in nums) {
    if (n % 2 == 0) {
      for (var l in letters) {
        pairs.add({'n': n, 'l': l});
      }
    }
  }
  print('--- Even pairs ---');
  for (var p in pairs) {
    print('${p['n']} ${p['l']}');
  }
}
