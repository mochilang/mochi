void main() {
  var customers = [
    {'id': 1, 'name': 'Alice'},
    {'id': 2, 'name': 'Bob'},
  ];
  var orders = [
    {'id': 100, 'customerId': 1},
    {'id': 101, 'customerId': 1},
    {'id': 102, 'customerId': 2},
  ];

  var joined = [];
  for (var o in orders) {
    var customer = customers.firstWhere((c) => c['id'] == o['customerId']);
    joined.add({'name': customer['name'], 'orderId': o['id']});
  }

  var groups = <String, List<Map<String, dynamic>>>{};
  for (var j in joined) {
    groups.putIfAbsent(j['name'], () => []).add(j);
  }

  var stats = [];
  groups.forEach((name, list) {
    stats.add({'name': name, 'count': list.length});
  });

  print('--- Orders per customer ---');
  for (var s in stats) {
    print('${s['name']} orders: ${s['count']}');
  }
}
