void main() {
  var xs = [1, 2, 3];
  var ys = xs.where((x) => x % 2 == 1).toList();
  print(ys.contains(1));
  print(ys.contains(2));

  var m = {'a': 1};
  print(m.containsKey('a'));
  print(m.containsKey('b'));

  var s = 'hello';
  print(s.contains('ell'));
  print(s.contains('foo'));
}
