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

num sqrtApprox(num x) {
  num guess = x;
  int i = 0;
  while (i < 20) {
    guess = (guess + x / guess) / 2.0;
    i = i + 1;
  }
  return guess;
}

Map<String, dynamic> makeSym(int order, List<num> elements) {
  return {"order": order, "ele": elements};
}

List<List<num>> unpackSym(Map<String, dynamic> m) {
  var n = m["order"]!;
  var ele = m["ele"]!;
  List<List<num>> mat = <List<num>>[];
  int idx = 0;
  int r = 0;
  while (r.toString().compareTo(n.toString()) < 0) {
    List<num> row = <num>[];
    int c = 0;
    while (c <= r) {
    row = [...row, ele[idx]];
    idx = idx + 1;
    c = c + 1;
  }
    while (c.toString().compareTo(n.toString()) < 0) {
    row = [...row, 0.0];
    c = c + 1;
  }
    mat = [...mat, row];
    r = r + 1;
  }
  r = 0;
  while (r.toString().compareTo(n.toString()) < 0) {
    int c = r + 1;
    while (c.toString().compareTo(n.toString()) < 0) {
    mat[r]![c] = mat[c][r];
    c = c + 1;
  }
    r = r + 1;
  }
  return mat;
}

void printMat(List<List<num>> m) {
  int i = 0;
  while (i < m.length) {
    String line = "";
    int j = 0;
    while (j < m[i].length) {
    line = line + (m[i][j]).toString();
    if (j < m[i].length - 1) {
    line = line + " ";
  }
    j = j + 1;
  }
    print(line);
    i = i + 1;
  }
}

void printSym(Map<String, dynamic> m) {
  printMat(unpackSym(m));
}

void printLower(Map<String, dynamic> m) {
  var n = m["order"]!;
  var ele = m["ele"]!;
  List<List<num>> mat = <List<num>>[];
  int idx = 0;
  int r = 0;
  while (r.toString().compareTo(n.toString()) < 0) {
    List<num> row = <num>[];
    int c = 0;
    while (c <= r) {
    row = [...row, ele[idx]];
    idx = idx + 1;
    c = c + 1;
  }
    while (c.toString().compareTo(n.toString()) < 0) {
    row = [...row, 0.0];
    c = c + 1;
  }
    mat = [...mat, row];
    r = r + 1;
  }
  printMat(mat);
}

Map<String, dynamic> choleskyLower(Map<String, dynamic> a) {
  var n = a["order"]!;
  var ae = a["ele"]!;
  List<num> le = <num>[];
  int idx = 0;
  while (idx < ae.length) {
    le = [...le, 0.0];
    idx = idx + 1;
  }
  int row = 1;
  int col = 1;
  int dr = 0;
  int dc = 0;
  int i = 0;
  while (i < ae.length) {
    var e = ae[i];
    if (i < dr) {
    num d = (e - le[i]) / le[dc];
    le[i] = d;
    int ci = col;
    int cx = dc;
    int j = i + 1;
    while (j <= dr) {
    cx = cx + ci;
    ci = ci + 1;
    le[j] = le[j] + d * le[cx];
    j = j + 1;
  };
    col = col + 1;
    dc = dc + col;
  } else {
    le[i] = sqrtApprox(e - le[i]);
    row = row + 1;
    dr = dr + row;
    col = 1;
    dc = 0;
  }
    i = i + 1;
  }
  return {"order": n, "ele": le};
}

void demo(Map<String, dynamic> a) {
  print("A:");
  printSym(a);
  print("L:");
  Map<String, dynamic> l = choleskyLower(a);
  printLower(l);
}

void main() {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _initNow();
  {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  demo(makeSym(3, [25.0, 15.0, 18.0, -5.0, 0.0, 11.0]));
  demo(makeSym(4, [18.0, 22.0, 70.0, 54.0, 86.0, 174.0, 42.0, 62.0, 134.0, 106.0]));
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
