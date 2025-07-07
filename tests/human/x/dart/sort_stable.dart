void main() {
  var items = [
    {'n': 1, 'v': 'a'},
    {'n': 1, 'v': 'b'},
    {'n': 2, 'v': 'c'}
  ];
  var result = List.from(items)..sort((a, b) => (a['n'] as int).compareTo(b['n'] as int));
  print(result.map((e) => e['v']).toList());
}
