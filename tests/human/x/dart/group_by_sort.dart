void main() {
  var items = [
    {'cat': 'a', 'val': 3},
    {'cat': 'a', 'val': 1},
    {'cat': 'b', 'val': 5},
    {'cat': 'b', 'val': 2}
  ];

  var groups = <String, List<Map<String, dynamic>>>{};
  for (var i in items) {
    groups.putIfAbsent(i['cat'], () => []).add(i);
  }

  var result = [];
  groups.forEach((cat, list) {
    var total = list.fold(0, (a, b) => a + (b['val'] as int));
    result.add({'cat': cat, 'total': total});
  });

  result.sort((a, b) => (b['total'] as int).compareTo(a['total']));

  print(result);
}
