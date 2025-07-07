int sumRec(int n, int acc) {
  if (n == 0) {
    return acc;
  }
  return sumRec(n - 1, acc + n);
}

void main() {
  print(sumRec(10, 0));
}
