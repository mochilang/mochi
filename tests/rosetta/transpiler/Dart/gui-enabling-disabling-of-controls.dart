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

class State {
  bool entry;
  bool inc;
  bool dec;
  State({required this.entry, required this.inc, required this.dec});
}

State state(int v) {
  return State(entry: v == 0, inc: v < 10, dec: v > 0);
}

void printState(int v) {
  dynamic s = state(v);
  print("value=" + (v).toString() + " entry=" + (s.entry).toString() + " inc=" + (s.inc).toString() + " dec=" + (s.dec).toString());
}

void _main() {
  dynamic v = 0;
  printState(v);
  while (true) {
    dynamic s = state(v);
    if (!s.inc) {
    break;
  }
    v = v + 1;
    printState(v);
  }
  while (true) {
    dynamic s = state(v);
    if (!s.dec) {
    break;
  }
    v = v - 1;
    printState(v);
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
