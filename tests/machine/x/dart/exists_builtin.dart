void main() {
  var data = [1, 2];
  var flag = (() {
    var _q0 = <dynamic>[];
    for (var x in data) {
      if (!(x == 1)) continue;
      _q0.add(x);
    }
    return _q0;
  })().isNotEmpty;
  print(flag);
}
