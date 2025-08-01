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

int absInt(int n) {
  if (n < 0) {
    return -n;
  }
  return n;
}

void _main() {
  int b = 1;
  while (b <= 5) {
    if (b != 5) {
    int c = 1;
    while (c <= 5) {
    if (c != 1 && c != b) {
    int f = 1;
    while (f <= 5) {
    if (f != 1 && f != 5 && f != b && f != c && absInt(f - c) > 1) {
    int m = 1;
    while (m <= 5) {
    if (m != b && m != c && m != f && m > c) {
    int s = 1;
    while (s <= 5) {
    if (s != b && s != c && s != f && s != m && absInt(s - f) > 1) {
    print("Baker in " + (b).toString() + ", Cooper in " + (c).toString() + ", Fletcher in " + (f).toString() + ", Miller in " + (m).toString() + ", Smith in " + (s).toString() + ".");
    return;
  }
    s = s + 1;
  };
  }
    m = m + 1;
  };
  }
    f = f + 1;
  };
  }
    c = c + 1;
  };
  }
    b = b + 1;
  }
  print("No solution found.");
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
