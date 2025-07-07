void main() {
  var data = [
    {'a': 1, 'b': 2},
    {'a': 1, 'b': 1},
    {'a': 0, 'b': 5}
  ];
  var sorted = List.from(data)
    ..sort((x, y) {
      var cmpA = (x['a'] as int).compareTo(y['a'] as int);
      if (cmpA != 0) return cmpA;
      return (x['b'] as int).compareTo(y['b'] as int);
    });
  print(sorted);
}
