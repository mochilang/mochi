void main() {
  var data = [
    {'tag': 'a', 'val': 1},
    {'tag': 'a', 'val': 2},
    {'tag': 'b', 'val': 3}
  ];

  var groups = <String, List<Map<String, dynamic>>>{};
  for (var d in data) {
    groups.putIfAbsent(d['tag'], () => []).add(d);
  }

  var tmp = [];
  groups.forEach((tag, items) {
    var total = 0;
    for (var x in items) {
      total += x['val'] as int;
    }
    tmp.add({'tag': tag, 'total': total});
  });

  tmp.sort((a, b) => (a['tag'] as String).compareTo(b['tag'] as String));
  print(tmp);
}
