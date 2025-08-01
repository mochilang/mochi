// Generated by Mochi transpiler
import 'dart:convert';
import 'dart:io';

int _nowSeed = 0;
bool _nowSeeded = false;
void _initNow() {
  var s = Platform.environment['MOCHI_NOW_SEED'];
  if (s != null && s.isNotEmpty) {
    var v = int.tryParse(s);
    if (v != null) {
      _nowSeed = v;
      _nowSeeded = true;
    }
  }
}
int _now() {
  if (_nowSeeded) {
    _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647;
    return _nowSeed;
  }
  return DateTime.now().microsecondsSinceEpoch;
}

String _substr(String s, int start, int end) {
  var n = s.length;
  if (start < 0) start += n;
  if (end < 0) end += n;
  if (start < 0) start = 0;
  if (start > n) start = n;
  if (end < 0) end = 0;
  if (end > n) end = n;
  if (start > end) start = end;
  return s.substring(start, end);
}

num pow10(int n) {
  num r = 1.0;
  int i = 0;
  while (i < n) {
    r = r * 10.0;
    i = i + 1;
  }
  return r;
}

num powf(num base, num exp) {
  if (exp == 0.5) {
    num guess = base;
    int i = 0;
    while (i < 20) {
    guess = (guess + base / guess) / 2.0;
    i = i + 1;
  };
    return guess;
  }
  num result = 1.0;
  int n = (exp).toInt();
  int i = 0;
  while (i < n) {
    result = result * base;
    i = i + 1;
  }
  return result;
}

String formatFloat(num f, int prec) {
  num scale = pow10(prec);
  num scaled = f * scale + 0.5;
  int n = (scaled).toInt();
  String digits = (n).toString();
  while (digits.length <= prec) {
    digits = "0" + digits;
  }
  String intPart = _substr(digits, 0, digits.length - prec);
  String fracPart = _substr(digits, digits.length - prec, digits.length);
  return intPart + "." + fracPart;
}

String padLeft(String s, int w) {
  String res = "";
  int n = w - s.length;
  while (n > 0) {
    res = res + " ";
    n = n - 1;
  }
  return res + s;
}

String rowString(List<num> row) {
  String s = "[";
  int i = 0;
  while (i < row.length) {
    s = s + padLeft(formatFloat(row[i], 3), 6);
    if (i < row.length - 1) {
    s = s + " ";
  }
    i = i + 1;
  }
  return s + "] ";
}

void printMatrix(String heading, List<List<num>> m) {
  print(heading);
  int i = 0;
  while (i < m.length) {
    print(rowString(m[i]));
    i = i + 1;
  }
}

List<List<num>> elementWiseMM(List<List<num>> m1, List<List<num>> m2, dynamic f) {
  List<List<num>> z = <List<num>>[];
  int r = 0;
  while (r < m1.length) {
    List<num> row = <num>[];
    int c = 0;
    while (c < m1[r].length) {
    row = [...row, f(m1[r][c], m2[r][c])];
    c = c + 1;
  }
    z = [...z, row];
    r = r + 1;
  }
  return z;
}

List<List<num>> elementWiseMS(List<List<num>> m, num s, dynamic f) {
  List<List<num>> z = <List<num>>[];
  int r = 0;
  while (r < m.length) {
    List<num> row = <num>[];
    int c = 0;
    while (c < m[r].length) {
    row = [...row, f(m[r][c], s)];
    c = c + 1;
  }
    z = [...z, row];
    r = r + 1;
  }
  return z;
}

num add(num a, num b) {
  return a + b;
}

num sub(num a, num b) {
  return a - b;
}

num mul(num a, num b) {
  return a * b;
}

num div(num a, num b) {
  return a / b;
}

num exp(num a, num b) {
  return powf(a, b);
}

void _main() {
  List<List<num>> m1 = [[3.0, 1.0, 4.0], [1.0, 5.0, 9.0]];
  List<List<num>> m2 = [[2.0, 7.0, 1.0], [8.0, 2.0, 8.0]];
  printMatrix("m1:", m1);
  printMatrix("m2:", m2);
  print("");
  printMatrix("m1 + m2:", elementWiseMM(m1, m2, add));
  printMatrix("m1 - m2:", elementWiseMM(m1, m2, sub));
  printMatrix("m1 * m2:", elementWiseMM(m1, m2, mul));
  printMatrix("m1 / m2:", elementWiseMM(m1, m2, div));
  printMatrix("m1 ^ m2:", elementWiseMM(m1, m2, exp));
  print("");
  num s = 0.5;
  print("s: " + (s).toString());
  printMatrix("m1 + s:", elementWiseMS(m1, s, add));
  printMatrix("m1 - s:", elementWiseMS(m1, s, sub));
  printMatrix("m1 * s:", elementWiseMS(m1, s, mul));
  printMatrix("m1 / s:", elementWiseMS(m1, s, div));
  printMatrix("m1 ^ s:", elementWiseMS(m1, s, exp));
}

void _start() {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _initNow();
  {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _main();
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "_start"}));
}

void main() => _start();
