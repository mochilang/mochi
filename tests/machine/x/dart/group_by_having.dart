import 'dart:convert';

void main() {
  var people = [{'name': 'Alice', 'city': 'Paris'}, {'name': 'Bob', 'city': 'Hanoi'}, {'name': 'Charlie', 'city': 'Paris'}, {'name': 'Diana', 'city': 'Hanoi'}, {'name': 'Eve', 'city': 'Paris'}, {'name': 'Frank', 'city': 'Hanoi'}, {'name': 'George', 'city': 'Paris'}];
  var big = (() {
  var _q0 = <dynamic>[];
  for (var p in people) {
    _q0.add({'city': g.key, 'num': g.length});
  }
  return _q0;
})();
  print(jsonEncode(big));
}
