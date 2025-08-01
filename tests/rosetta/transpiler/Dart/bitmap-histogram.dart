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

List<List<int>> image() {
  return [[0, 0, 10000], [65535, 65535, 65535], [65535, 65535, 65535]];
}

List<int> histogram(List<List<int>> g, int bins) {
  if (bins <= 0) {
    bins = g[0].length;
  }
  List<int> h = <int>[];
  int i = 0;
  while (i < bins) {
    h = [...h, 0];
    i = i + 1;
  }
  int y = 0;
  while (y < g.length) {
    List<int> row = g[y];
    int x = 0;
    while (x < row.length) {
    int p = row[x];
    int idx = p * (bins - 1) ~/ 65535 as int;
    h[idx] = h[idx] + 1;
    x = x + 1;
  }
    y = y + 1;
  }
  return h;
}

int medianThreshold(List<int> h) {
  int lb = 0;
  int ub = h.length - 1;
  int lSum = 0;
  int uSum = 0;
  while (lb <= ub) {
    if (lSum + h[lb] < uSum + h[ub]) {
    lSum = lSum + h[lb];
    lb = lb + 1;
  } else {
    uSum = uSum + h[ub];
    ub = ub - 1;
  }
  }
  return ub * 65535 ~/ h.length as int;
}

List<List<int>> threshold(List<List<int>> g, int t) {
  List<List<int>> out = <List<int>>[];
  int y = 0;
  while (y < g.length) {
    List<int> row = g[y];
    List<int> newRow = <int>[];
    int x = 0;
    while (x < row.length) {
    if (row[x] < t) {
    newRow = [...newRow, 0];
  } else {
    newRow = [...newRow, 65535];
  }
    x = x + 1;
  }
    out = ([...out, newRow] as List).map((e) => (e as List).map((e) => (e is BigInt ? e.toInt() : (e as int))).toList()).toList();
    y = y + 1;
  }
  return out;
}

void printImage(List<List<int>> g) {
  int y = 0;
  while (y < g.length) {
    List<int> row = g[y];
    String line = "";
    int x = 0;
    while (x < row.length) {
    if (row[x] == 0) {
    line = line + "0";
  } else {
    line = line + "1";
  }
    x = x + 1;
  }
    print(line);
    y = y + 1;
  }
}

void _main() {
  List<List<int>> img = image();
  List<int> h = histogram(img, 0);
  print("Histogram: " + (h).toString());
  int t = medianThreshold(h);
  print("Threshold: " + (t).toString());
  List<List<int>> bw = threshold(img, t);
  printImage(bw);
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
