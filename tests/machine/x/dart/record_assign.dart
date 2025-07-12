class Counter {
  int n;
  Counter(this.n);
}

void inc(Counter c) {
  c.n = ((c.n + 1 as int) as int);
}

var c = Counter(0);

void main() {
  inc(c);
  print(c.n);
}
