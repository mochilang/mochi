void main() {
  var nations = [
    {'id': 1, 'name': 'A'},
    {'id': 2, 'name': 'B'}
  ];
  var suppliers = [
    {'id': 1, 'nation': 1},
    {'id': 2, 'nation': 2}
  ];
  var partsupp = [
    {'part': 100, 'supplier': 1, 'cost': 10.0, 'qty': 2},
    {'part': 100, 'supplier': 2, 'cost': 20.0, 'qty': 1},
    {'part': 200, 'supplier': 1, 'cost': 5.0, 'qty': 3}
  ];

  var filtered = [];
  for (var ps in partsupp) {
    var s = suppliers.firstWhere((x) => x['id'] == ps['supplier']);
    var n = nations.firstWhere((x) => x['id'] == s['nation']);
    if (n['name'] == 'A') {
      filtered.add({'part': ps['part'], 'value': ps['cost'] * ps['qty']});
    }
  }

  var groups = <int, List<Map<String, dynamic>>>{};
  for (var f in filtered) {
    groups.putIfAbsent(f['part'], () => []).add(f);
  }

  var grouped = [];
  groups.forEach((part, list) {
    var total = list.fold(0.0, (a, b) => a + (b['value'] as num));
    grouped.add({'part': part, 'total': total});
  });

  print(grouped);
}
