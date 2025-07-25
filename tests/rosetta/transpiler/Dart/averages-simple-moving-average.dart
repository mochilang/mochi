// Generated by Mochi transpiler
int indexOf(String s, String ch) {
  int i = 0;
  while (i < s.length) {
    if (s.substring(i, i + 1) == ch) {
    return i;
  }
    i = i + 1;
  }
  return -1;
}

String fmt3(num x) {
  num y = (((x * 1000.0 + 0.5).toInt()).toDouble()) / 1000.0;
  String s = (y).toString();
  int dot = indexOf(s, ".");
  if (dot == 0 - 1) {
    s = s + ".000";
  } else {
    int decs = s.length - dot - 1;
    if (decs > 3) {
    s = s.substring(0, dot + 4);
  } else {
    while (decs < 3) {
    s = s + "0";
    decs = decs + 1;
  };
  };
  }
  return s;
}

String pad(String s, int width) {
  String out = s;
  while (out.length < width) {
    out = " " + out;
  }
  return out;
}

List<num> smaSeries(List<num> xs, int period) {
  List<num> res = [];
  num sum = 0.0;
  int i = 0;
  while (i < xs.length) {
    sum = sum + xs[i];
    if (i >= period) {
    sum = sum - xs[i - period];
  }
    int denom = i + 1;
    if (denom > period) {
    denom = period;
  }
    res = [...res, sum / ((denom).toDouble())];
    i = i + 1;
  }
  return res;
}

void main() {
  List<num> xs = [1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 2.0, 1.0];
  List<num> sma3 = smaSeries(xs, 3);
  List<num> sma5 = smaSeries(xs, 5);
  print("x       sma3   sma5");
  int i = 0;
  while (i < xs.length) {
    final String line = pad(fmt3(xs[i]), 5) + "  " + pad(fmt3(sma3[i]), 5) + "  " + pad(fmt3(sma5[i]), 5);
    print(line);
    i = i + 1;
  }
}

void _start() {
  main();
}
