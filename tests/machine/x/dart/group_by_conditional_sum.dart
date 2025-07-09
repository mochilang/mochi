void main() {
  var items = [
    {'cat': 'a', 'val': 10, 'flag': true},
    {'cat': 'a', 'val': 5, 'flag': false},
    {'cat': 'b', 'val': 20, 'flag': true},
  ];
  var result = (() {
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
        _k4,
        {
          'cat': _k4,
          'share':
              (() {
                var _q5 = <dynamic>[];
                for (var x in g) {
                  _q5.add((x['flag'] ? x['val'] : 0));
                }
                return _q5;
              })().reduce((a, b) => a + b) /
              (() {
                var _q6 = <dynamic>[];
                for (var x in g) {
                  _q6.add(x['val']);
                }
                return _q6;
              })().reduce((a, b) => a + b),
        },
      ]);
    }
    _q0.sort((a, b) => (a[0] as Comparable).compareTo(b[0]));
    _q0 = [for (var x in _q0) x[1]];
    return _q0;
  })();
  print(result);
}
