var scores = {'alice': 1};

void main() {
  scores['bob'] = (2 as int);
  print((scores as Map)['bob']);
}
