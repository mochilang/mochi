var data = {
  'outer': {'inner': 1},
};

void main() {
  (data['outer'] as Map)['inner'] = 2;
  print(((data as Map)['outer'] as Map)['inner']);
}
