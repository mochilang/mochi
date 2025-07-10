var items = [
  {'cat': 'a', 'val': 3},
  {'cat': 'a', 'val': 1},
  {'cat': 'b', 'val': 5},
  {'cat': 'b', 'val': 2},
];

var grouped = (() {
  var _q0 = <dynamic>[];
  var _g1 = <dynamic, List<dynamic>>{};
  for (var i in items) {
    var _k4 = i['cat'];
    _g1.putIfAbsent(_k4, () => <dynamic>[]).add(i);
  }
  for (var entry in _g1.entries) {
    var g = entry.value;
    var _k4 = entry.key;
    _q0.add([
      -(() {
        var _t8 = (() {
          var _q7 = <dynamic>[];
          for (var x in g) {
            _q7.add(x['val']);
          }
          return _q7;
        })();
        return _t8.reduce((a, b) => a + b);
      })(),
      {
        'cat': _k4,
        'total': (() {
          var _t6 = (() {
            var _q5 = <dynamic>[];
            for (var x in g) {
              _q5.add(x['val']);
            }
            return _q5;
          })();
          return _t6.reduce((a, b) => a + b);
        })(),
      },
    ]);
  }
  _q0.sort((a, b) => (a[0] as Comparable).compareTo(b[0]));
  _q0 = [for (var x in _q0) x[1]];
  return _q0;
})();

void main() {
  print(grouped);
}
