void main() {
  var nation = [
    {'n_nationkey': 1, 'n_name': 'BRAZIL'}
  ];
  var customer = [
    {
      'c_custkey': 1,
      'c_name': 'Alice',
      'c_acctbal': 100.0,
      'c_nationkey': 1,
      'c_address': '123 St',
      'c_phone': '123-456',
      'c_comment': 'Loyal'
    }
  ];
  var orders = [
    {'o_orderkey': 1000, 'o_custkey': 1, 'o_orderdate': '1993-10-15'},
    {'o_orderkey': 2000, 'o_custkey': 1, 'o_orderdate': '1994-01-02'}
  ];
  var lineitem = [
    {'l_orderkey': 1000, 'l_returnflag': 'R', 'l_extendedprice': 1000.0, 'l_discount': 0.1},
    {'l_orderkey': 2000, 'l_returnflag': 'N', 'l_extendedprice': 500.0, 'l_discount': 0.0}
  ];

  var startDate = '1993-10-01';
  var endDate = '1994-01-01';

  var rows = [];
  for (var c in customer) {
    for (var o in orders.where((o) => o['o_custkey'] == c['c_custkey'])) {
      if (o['o_orderdate'].compareTo(startDate) >= 0 &&
          o['o_orderdate'].compareTo(endDate) < 0) {
        for (var l in lineitem.where((l) => l['l_orderkey'] == o['o_orderkey'])) {
          if (l['l_returnflag'] == 'R') {
            var n = nation.firstWhere((n) => n['n_nationkey'] == c['c_nationkey']);
            rows.add({
              'c': c,
              'o': o,
              'l': l,
              'n': n,
            });
          }
        }
      }
    }
  }

  var groups = <String, List<Map<String, dynamic>>>{};
  for (var r in rows) {
    var key = [
      r['c']['c_custkey'],
      r['c']['c_name'],
      r['c']['c_acctbal'],
      r['c']['c_address'],
      r['c']['c_phone'],
      r['c']['c_comment'],
      r['n']['n_name']
    ].join('|');
    groups.putIfAbsent(key, () => []).add(r);
  }

  var result = [];
  groups.forEach((key, list) {
    var revenue = list.fold(0.0, (a, b) {
      var l = b['l'];
      return a + l['l_extendedprice'] * (1 - l['l_discount']);
    });
    var parts = key.split('|');
    result.add({
      'c_custkey': int.parse(parts[0]),
      'c_name': parts[1],
      'revenue': revenue,
      'c_acctbal': double.parse(parts[2]),
      'n_name': parts[6],
      'c_address': parts[3],
      'c_phone': parts[4],
      'c_comment': parts[5]
    });
  });

  result.sort((a, b) => (b['revenue'] as num).compareTo(a['revenue']));

  print(result);
}
