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

num det(List<List<num>> m) {
  int n = m.length;
  if (n == 1) {
    return m[0][0];
  }
  num total = 0.0;
  num sign = 1.0;
  int c = 0;
  while (c < n) {
    List<List<num>> sub = <List<num>>[];
    int r = 1;
    while (r < n) {
    List<num> row = <num>[];
    int cc = 0;
    while (cc < n) {
    if (cc != c) {
    row = [...row, m[r][cc]];
  }
    cc = cc + 1;
  }
    sub = [...sub, row];
    r = r + 1;
  }
    total = total + sign * m[0][c] * det(sub);
    sign = sign * -1.0;
    c = c + 1;
  }
  return total;
}

List<List<num>> replaceCol(List<List<num>> m, int col, List<num> v) {
  List<List<num>> res = <List<num>>[];
  int r = 0;
  while (r < m.length) {
    List<num> row = <num>[];
    int c = 0;
    while (c < m[r].length) {
    if (c == col) {
    row = [...row, v[r]];
  } else {
    row = [...row, m[r][c]];
  }
    c = c + 1;
  }
    res = [...res, row];
    r = r + 1;
  }
  return res;
}

List<List<num>> m = [[2.0, -1.0, 5.0, 1.0], [3.0, 2.0, 2.0, -6.0], [1.0, 3.0, 3.0, -1.0], [5.0, -2.0, -3.0, 3.0]];
List<num> v = [-3.0, -32.0, -47.0, 49.0];
num d = det(m);
List<num> x = <num>[];
int i = 0;
String s = "[";
int j = 0;
void main() {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _initNow();
  {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  while (i < v.length) {
    List<List<num>> mc = replaceCol(m, i, v);
    x = [...x, det(mc) / d];
    i = i + 1;
  }
  while (j < x.length) {
    s = s + (x[j]).toString();
    if (j < x.length - 1) {
    s = s + " ";
  }
    j = j + 1;
  }
  s = s + "]";
  print(s);
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
