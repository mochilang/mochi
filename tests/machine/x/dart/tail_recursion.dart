int sum_rec(int n, int acc) {
  if (n == 0) {
    return acc;
  }
  return sum_rec((n as num) - 1, (acc as num) + (n as num));
}

void main() {
  print(sum_rec(10, 0));
}
