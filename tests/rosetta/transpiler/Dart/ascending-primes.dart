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

List<int> asc = <int>[];
void gen(int first, int cand, int digits) {
  if (digits == 0) {
    if (isPrime(cand)) {
    asc = (asc + [cand] as List).map((e) => (e is BigInt ? e.toInt() : (e as int))).toList();
  };
    return;
  }
  int i = first;
  while (i < 10) {
    gen(i + 1, cand * 10 + i, digits - 1);
    i = i + 1;
  }
}

String pad(int n, int width) {
  String s = (n).toString();
  while (s.length < width) {
    s = " " + s;
  }
  return s;
}

void _main() {
  int digits = 1;
  while (digits < 10) {
    gen(1, 0, digits);
    digits = digits + 1;
  }
  print("There are " + (asc.length).toString() + " ascending primes, namely:");
  int i = 0;
  String line = "";
  while (i < asc.length) {
    line = line + pad(asc[i], 8) + " ";
    if ((i + 1) % 10 == 0) {
    print(_substr(line, 0, line.length - 1));
    line = "";
  }
    i = i + 1;
  }
  if (line.length > 0) {
    print(_substr(line, 0, line.length - 1));
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
