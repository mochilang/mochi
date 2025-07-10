var scores = {'alice': 1};

void main() {
  scores['bob'] = 2;
  print((scores as Map)['bob']);
}
