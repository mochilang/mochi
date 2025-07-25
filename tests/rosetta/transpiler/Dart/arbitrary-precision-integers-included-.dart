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
  int pow_int(int base, int exp) {
  int result = 1;
  int b = base;
  int e = exp;
  while (e > 0) {
    if (e % 2 == 1) {
    result = result * b;
  }
    b = b * b;
    e = e ~/ 2 as int;
  }
  return result;
}
  BigInt pow_big(BigInt base, int exp) {
  BigInt result = BigInt.from(1);
  BigInt b = base;
  int e = exp;
  while (e > 0) {
    if (e % 2 == 1) {
    result = result * b;
  }
    b = b * b;
    e = e ~/ 2 as int;
  }
  return result;
}
  int e1 = pow_int(3, 2);
  int e2 = pow_int(4, e1);
  BigInt base = BigInt.from(5);
  BigInt x = pow_big(base, e2);
  String s = (x).toString();
  print(["5^(4^(3^2)) has", s.length, "digits:", s.substring(0, 20), "...", s.substring(s.length - 20, s.length)].join(" "));
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
