void main() {
  var data = {'outer': {'inner': 1}};
  (data['outer'] as Map<String, dynamic>)['inner'] = 2;
  print((data['outer'] as Map)['inner']);
}
