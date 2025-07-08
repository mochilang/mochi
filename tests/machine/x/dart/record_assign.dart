class Counter {
  int n;
  Counter(this.n);
}

void inc(Counter c) {
  c.n = c.n + 1;
}

void main() {
  var c = Counter(0);
  inc(c);
  print(c.n);
}
