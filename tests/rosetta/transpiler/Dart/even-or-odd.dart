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

BigInt parseBigInt(String str) {
  int i = 0;
  bool neg = false;
  if (str.length > 0 && _substr(str, 0, 1) == "-") {
    neg = true;
    i = 1;
  }
  BigInt n = BigInt.from(0);
  while (i < str.length) {
    String ch = _substr(str, i, i + 1);
    int d = (ch).codeUnitAt(0);
    n = n * (BigInt.from(10)) + (BigInt.from(d));
    i = i + 1;
  }
  if (neg) {
    n = -n;
  }
  return n;
}

String pad(int n, int width) {
  String s = (n).toString();
  while (s.length < width) {
    s = " " + s;
  }
  return s;
}

void showInt(int n) {
  String line = "Testing integer " + pad(n, 3) + ":  ";
  if (n % 2 == 0) {
    line = line + "even ";
  } else {
    line = line + " odd ";
  }
  if (n % 2 == 0) {
    line = line + "even";
  } else {
    line = line + " odd";
  }
  print(line);
}

void showBig(String s) {
  BigInt b = parseBigInt(s);
  String line = "Testing big integer " + (b).toString() + ":  ";
  if (b % (BigInt.from(2)) == (BigInt.from(0))) {
    line = line + "even";
  } else {
    line = line + "odd";
  }
  print(line);
}

void _main() {
  showInt(-2);
  showInt(-1);
  showInt(0);
  showInt(1);
  showInt(2);
  showBig("-222222222222222222222222222222222222");
  showBig("-1");
  showBig("0");
  showBig("1");
  showBig("222222222222222222222222222222222222");
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
