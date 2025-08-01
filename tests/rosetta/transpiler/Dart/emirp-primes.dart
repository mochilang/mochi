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
  int d = 3;
  while (d * d <= n) {
    if (n % d == 0) {
    return false;
  }
    d = d + 2;
  }
  return true;
}

int revInt(int n) {
  int r = 0;
  int t = n;
  while (t > 0) {
    r = r * 10 + t % 10;
    t = t ~/ 10 as int;
  }
  return r;
}

void _main() {
  List<int> emirps = <int>[];
  int n = 2;
  while (emirps.length < 10000) {
    if (isPrime(n)) {
    int r = revInt(n);
    if (r != n && isPrime(r)) {
    emirps = [...emirps, n];
  };
  }
    n = n + 1;
  }
  String line = "   [";
  int i = 0;
  while (i < 20) {
    line = line + (emirps[i]).toString();
    if (i < 19) {
    line = line + ", ";
  }
    i = i + 1;
  }
  line = line + "]";
  print("First 20:");
  print(line);
  line = "  [";
  for (int e in emirps) {
    if (e >= 8000) {
    break;
  }
    if (e >= 7700) {
    line = line + (e).toString() + ", ";
  }
  }
  line = line + "]";
  print("Between 7700 and 8000:");
  print(line);
  print("10000th:");
  print("   [" + (emirps[9999]).toString() + "]");
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
