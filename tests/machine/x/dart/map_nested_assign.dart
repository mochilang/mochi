void main() {
  var data = {'outer': {'inner': 1}};
  (data['outer'] as Map)['inner'] = 2;
  print(data['outer']['inner']);
}
