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
    {'orderId': 100, 'sku': 'a'},
    {'orderId': 101, 'sku': 'b'}
  ];

  var result = [];
  for (var o in orders) {
    var c = customers.firstWhere((c) => c['id'] == o['customerId']);
    var i = items.firstWhere((it) => it['orderId'] == o['id']);
    result.add({'name': c['name'], 'sku': i['sku']});
  }

  print('--- Multi Join ---');
  for (var r in result) {
    print('${r['name']} bought item ${r['sku']}');
  }
}
