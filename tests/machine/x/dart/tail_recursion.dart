int sum_rec(int n, int acc) {
  if (n == 0) {
    return acc;
  }
  return sum_rec(n - 1, acc + n);
}

void main() {
  print(sum_rec(10, 0));
}
