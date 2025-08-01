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

bool isPrime(int n) {
  if (n < 2) {
    return false;
  }
  if (n % 2 == 0) {
    return n == 2;
  }
  if (n % 3 == 0) {
    return n == 3;
  }
  int d = 5;
  while (d * d <= n) {
    if (n % d == 0) {
    return false;
  }
    d = d + 2;
    if (n % d == 0) {
    return false;
  }
    d = d + 4;
  }
  return true;
}

int countPrimeFactors(int n) {
  if (n == 1) {
    return 0;
  }
  if (isPrime(n)) {
    return 1;
  }
  int count = 0;
  int f = 2;
  while (true) {
    if (n % f == 0) {
    count = count + 1;
    n = n ~/ f;
    if (n == 1) {
    return count;
  };
    if (isPrime(n)) {
    f = n;
  };
  } else {
    if (f >= 3) {
    f = f + 2;
  } else {
    f = 3;
  };
  }
  }
  return count;
}

String pad4(int n) {
  String s = (n).toString();
  while (s.length < 4) {
    s = " " + s;
  }
  return s;
}

void _main() {
  int max = 120;
  print("The attractive numbers up to and including " + (max).toString() + " are:");
  int count = 0;
  String line = "";
  int lineCount = 0;
  int i = 1;
  while (i <= max) {
    int c = countPrimeFactors(i);
    if (isPrime(c)) {
    line = line + pad4(i);
    count = count + 1;
    lineCount = lineCount + 1;
    if (lineCount == 20) {
    print(line);
    line = "";
    lineCount = 0;
  };
  }
    i = i + 1;
  }
  if (lineCount > 0) {
    print(line);
  }
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
