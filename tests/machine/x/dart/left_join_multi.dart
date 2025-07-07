void main() {
  var customers = [
    {'id': 1, 'name': 'Alice'},
    {'id': 2, 'name': 'Bob'}
  ];

  var orders = [
    {'id': 100, 'customerId': 1},
    {'id': 101, 'customerId': 2}
  ];

  var items = [
    {'orderId': 100, 'sku': 'a'}
  ];

  var result = [];
  for (var o in orders) {
    var c = customers.firstWhere((c) => c['id'] == o['customerId']);
    var i = items.where((it) => it['orderId'] == o['id']).toList();
    var item = i.isNotEmpty ? i.first : null;
    result.add({'orderId': o['id'], 'name': c['name'], 'item': item});
  }

  print('--- Left Join Multi ---');
  for (var r in result) {
    print('${r['orderId']} ${r['name']} ${r['item']}');
  }
}
