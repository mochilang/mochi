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

class Rsdv {
  num n;
  num a;
  num q;
  Rsdv({required this.n, required this.a, required this.q});
}

num sqrtApprox(num x) {
  if (x <= 0.0) {
    return 0.0;
  }
  num g = x;
  int i = 0;
  while (i < 20) {
    g = (g + x / g) / 2.0;
    i = i + 1;
  }
  return g;
}

Rsdv newRsdv() {
  return Rsdv(n: 0.0, a: 0.0, q: 0.0);
}

Rsdv add(Rsdv r, num x) {
  num n1 = r.n + 1.0;
  num a1 = r.a + (x - r.a) / n1;
  num q1 = r.q + (x - r.a) * (x - a1);
  return Rsdv(n: n1, a: a1, q: q1);
}

num sd(Rsdv r) {
  return sqrtApprox(r.q / r.n);
}

void _main() {
  Rsdv r = newRsdv();
  for (num x in [2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0]) {
    r = add(r, x);
    print((sd(r)).toString());
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
