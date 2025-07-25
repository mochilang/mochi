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

void main() {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _initNow();
  {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  int fib(int n) {
  if (n < 2) {
    return n;
  }
  int a = 0;
  int b = 1;
  int i = 1;
  while (i < n) {
    final int t = a + b;
    a = b;
    b = t;
    i = i + 1;
  }
  return b;
}
  void main() {
  for (var i in [-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
    if (i < 0) {
    print("fib(" + (i).toString() + ") returned error: negative n is forbidden");
  } else {
    print("fib(" + (i).toString() + ") = " + (fib(i)).toString());
  }
  }
}
  main();
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
