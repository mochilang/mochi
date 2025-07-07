void main() {
  var customers = [
    {'id': 1, 'name': 'Alice'},
    {'id': 2, 'name': 'Bob'},
    {'id': 3, 'name': 'Charlie'}
  ];
  var orders = [
    {'id': 100, 'customerId': 1},
    {'id': 101, 'customerId': 1},
    {'id': 102, 'customerId': 2}
  ];

  var stats = [];
  for (var c in customers) {
    var count = orders.where((o) => o['customerId'] == c['id']).length;
    stats.add({'name': c['name'], 'count': count});
  }

  print('--- Group Left Join ---');
  for (var s in stats) {
    print('${s['name']} orders: ${s['count']}');
  }
}
