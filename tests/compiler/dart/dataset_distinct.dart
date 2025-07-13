List<int> nums = [1, 2, 2, 3, 1, 3];

List<int> unique = (() {
  var _res = [];
  for (var n in nums) {
    _res.add(n);
  }
  _res = _distinct(_res);
  return _res;
})();

void main() {
  print(unique);
}

List<dynamic> _distinct(List<dynamic> items) {
    var res = <dynamic>[];
    outer: for (var it in items) {
        for (var r in res) { if (_equal(r, it)) continue outer; }
        res.add(it);
    }
    return res;
}
