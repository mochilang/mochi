var x = 3;

var y = 4;

var m = {'a': x, 'b': y};

void main() {
  print([(m as Map)['a'], (m as Map)['b']].join(' '));
}
