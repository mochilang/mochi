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

num pow10(int n) {
  num r = 1.0;
  int i = 0;
  while (i < n) {
    r = r * 10.0;
    i = i + 1;
  }
  return r;
}

String formatFloat(num f, int prec) {
  num scale = pow10(prec);
  num scaled = f * scale + 0.5;
  int n = (scaled).toInt();
  String digits = (n).toString();
  while (digits.length <= prec) {
    digits = "0" + digits;
  }
  String intPart = _substr(digits, 0, digits.length - prec);
  String fracPart = _substr(digits, digits.length - prec, digits.length);
  return intPart + "." + fracPart;
}

String padLeftZeros(String s, int width) {
  String out = s;
  while (out.length < width) {
    out = "0" + out;
  }
  return out;
}

void main() {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _initNow();
  {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  print(padLeftZeros(formatFloat(7.125, 3), 9));
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
