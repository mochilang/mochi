import 'dart:convert';

var items = [
  {'cat': 'a', 'val': 10, 'flag': true},
  {'cat': 'a', 'val': 5, 'flag': false},
  {'cat': 'b', 'val': 20, 'flag': true},
];

var result = (() {
  var _q0 = <dynamic>[];
  var _g1 = <dynamic, List<dynamic>>{};
  for (var i in items) {
    var _k6 = i['cat'];
    _g1.putIfAbsent(_k6, () => <dynamic>[]).add(i);
  }
  for (var entry in _g1.entries) {
    var g = entry.value;
    var _k6 = entry.key;
    _q0.add([
      _k6,
      {
        'cat': _k6,
        'share':
            (() {
              var _t8 = (() {
                var _q7 = <dynamic>[];
                for (var x in g) {
                  _q7.add((x['flag'] ? x['val'] : 0));
                }
                return _q7;
              })();
              return _t8.reduce((a, b) => a + b);
            })() /
            (() {
              var _t10 = (() {
                var _q9 = <dynamic>[];
                for (var x in g) {
                  _q9.add(x['val']);
                }
                return _q9;
              })();
              return _t10.reduce((a, b) => a + b);
            })(),
      },
    ]);
  }
  _q0.sort(
    (a, b) => (jsonEncode(a[0]) as Comparable).compareTo(jsonEncode(b[0])),
  );
  _q0 = [for (var x in _q0) x[1]];
  return _q0;
})();

void main() {
  print(result);
}
