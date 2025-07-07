void main() {
  var items = [
    {'cat': 'a', 'val': 10, 'flag': true},
    {'cat': 'a', 'val': 5, 'flag': false},
    {'cat': 'b', 'val': 20, 'flag': true},
  ];

  var groups = <String, List<Map<String, dynamic>>>{};
  for (var item in items) {
    groups.putIfAbsent(item['cat'], () => []).add(item);
  }

  var result = [];
  var keys = groups.keys.toList()..sort();
  for (var key in keys) {
    var g = groups[key]!;
    var sumFlag = g.fold(0, (a, b) => a + ((b['flag'] as bool) ? b['val'] as int : 0));
    var sumVal = g.fold(0, (a, b) => a + (b['val'] as int));
    result.add({'cat': key, 'share': sumFlag / sumVal});
  }

  print(result);
}
