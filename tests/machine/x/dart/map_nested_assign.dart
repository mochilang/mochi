var data = {
  'outer': {'inner': 1},
};

void main() {
  (data['outer'] as Map)['inner'] = (2 as int);
  print(((data as Map)['outer'] as Map)['inner']);
}
