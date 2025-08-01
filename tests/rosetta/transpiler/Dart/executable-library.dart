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

List<int> hailstone(int n) {
  List<int> seq = <int>[];
  int x = n;
  seq = [...seq, x];
  while (x > 1) {
    if (x % 2 == 0) {
    x = x ~/ 2;
  } else {
    x = 3 * x + 1;
  }
    seq = [...seq, x];
  }
  return seq;
}

String listString(List<int> xs) {
  String s = "[";
  int i = 0;
  while (i < xs.length) {
    s = s + (xs[i]).toString();
    if (i < xs.length - 1) {
    s = s + " ";
  }
    i = i + 1;
  }
  s = s + "]";
  return s;
}

void libMain() {
  List<int> seq = hailstone(27);
  print("");
  print("Hailstone sequence for the number 27:");
  print("  has " + (seq.length).toString() + " elements");
  print("  starts with " + listString(seq.sublist(0, 4)));
  print("  ends with " + listString(seq.sublist(seq.length - 4, seq.length)));
  int longest = 0;
  int length = 0;
  int i = 1;
  while (i < 100000) {
    int l = hailstone(i).length;
    if (l > length) {
    longest = i;
    length = l;
  }
    i = i + 1;
  }
  print("");
  print((longest).toString() + " has the longest Hailstone sequence, its length being " + (length).toString() + ".");
}

void main() {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _initNow();
  {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  libMain();
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
