var nation = [
  {'n_nationkey': 1, 'n_name': 'BRAZIL'},
];

var customer = [
  {
    'c_custkey': 1,
    'c_name': 'Alice',
    'c_acctbal': 100,
    'c_nationkey': 1,
    'c_address': '123 St',
    'c_phone': '123-456',
    'c_comment': 'Loyal',
  },
];

var orders = [
  {'o_orderkey': 1000, 'o_custkey': 1, 'o_orderdate': '1993-10-15'},
  {'o_orderkey': 2000, 'o_custkey': 1, 'o_orderdate': '1994-01-02'},
];

var lineitem = [
  {
    'l_orderkey': 1000,
    'l_returnflag': 'R',
    'l_extendedprice': 1000,
    'l_discount': 0.1,
  },
  {
    'l_orderkey': 2000,
    'l_returnflag': 'N',
    'l_extendedprice': 500,
    'l_discount': 0,
  },
];

var start_date = '1993-10-01';

var end_date = '1994-01-01';

var result = (() {
  var _q0 = <dynamic>[];
  var _g1 = <dynamic, List<dynamic>>{};
  for (var c in customer) {
    for (var o in orders) {
      if (!(o['o_custkey'] == c['c_custkey'])) continue;
      for (var l in lineitem) {
        if (!(l['l_orderkey'] == o['o_orderkey'])) continue;
        for (var n in nation) {
          if (!(n['n_nationkey'] == c['c_nationkey'])) continue;
          if (!(((o['o_orderdate'] as num) >= (start_date as num) &&
                      o['o_orderdate'] as num) <
                  (end_date as num) &&
              l['l_returnflag'] == 'R'))
            continue;
          var _k4 = {
            'c_custkey': c['c_custkey'],
            'c_name': c['c_name'],
            'c_acctbal': c['c_acctbal'],
            'c_address': c['c_address'],
            'c_phone': c['c_phone'],
            'c_comment': c['c_comment'],
            'n_name': n['n_name'],
          };
          _g1.putIfAbsent(_k4, () => <dynamic>[]).add({
            'c': c,
            'o': o,
            'l': l,
            'n': n,
          });
        }
      }
    }
  }
  for (var entry in _g1.entries) {
    var g = entry.value;
    var _k4 = entry.key;
    _q0.add([
      -(() {
        var _t8 = (() {
          var _q7 = <dynamic>[];
          for (var x in g) {
            _q7.add(
              (x['l']['l_extendedprice'] as num) *
                  ((1 - (x['l']['l_discount'] as num)) as num),
            );
          }
          return _q7;
        })();
        return _t8.reduce((a, b) => a + b);
      })(),
      {
        'c_custkey': g['key']['c_custkey'],
        'c_name': g['key']['c_name'],
        'revenue': (() {
          var _t6 = (() {
            var _q5 = <dynamic>[];
            for (var x in g) {
              _q5.add(
                (x['l']['l_extendedprice'] as num) *
                    ((1 - (x['l']['l_discount'] as num)) as num),
              );
            }
            return _q5;
          })();
          return _t6.reduce((a, b) => a + b);
        })(),
        'c_acctbal': g['key']['c_acctbal'],
        'n_name': g['key']['n_name'],
        'c_address': g['key']['c_address'],
        'c_phone': g['key']['c_phone'],
        'c_comment': g['key']['c_comment'],
      },
    ]);
  }
  _q0.sort((a, b) => (a[0] as Comparable).compareTo(b[0]));
  _q0 = [for (var x in _q0) x[1]];
  return _q0;
})();

void main() {
  print(result);
}
