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

final num PI = 3.141592653589793;
num sinApprox(num x) {
  num term = x;
  num sum = x;
  int n = 1;
  while (n <= 8) {
    final num denom = (2 * n * (2 * n + 1)).toDouble();
    term = -term * x * x / denom;
    sum = sum + term;
    n = n + 1;
  }
  return sum;
}

num cosApprox(num x) {
  num term = 1.0;
  num sum = 1.0;
  int n = 1;
  while (n <= 8) {
    final num denom = ((2 * n - 1) * 2 * n).toDouble();
    term = -term * x * x / denom;
    sum = sum + term;
    n = n + 1;
  }
  return sum;
}

num atanApprox(num x) {
  if (x > 1.0) {
    return PI / 2.0 - x / (x * x + 0.28);
  }
  if (x < -1.0) {
    return -PI / 2.0 - x / (x * x + 0.28);
  }
  return x / (1.0 + 0.28 * x * x);
}

num atan2Approx(num y, num x) {
  if (x > 0.0) {
    return atanApprox(y / x);
  }
  if (x < 0.0) {
    if (y >= 0.0) {
    return atanApprox(y / x) + PI;
  };
    return atanApprox(y / x) - PI;
  }
  if (y > 0.0) {
    return PI / 2.0;
  }
  if (y < 0.0) {
    return -PI / 2.0;
  }
  return 0.0;
}

int digit(String ch) {
  final String digits = "0123456789";
  int i = 0;
  while (i < digits.length) {
    if (digits.substring(i, i + 1) == ch) {
    return i;
  }
    i = i + 1;
  }
  return 0;
}

int parseTwo(String s, int idx) {
  return digit(s.substring(idx, idx + 1)) * 10 + digit(s.substring(idx + 1, idx + 2));
}

num parseSec(String s) {
  final int h = parseTwo(s, 0);
  final int m = parseTwo(s, 3);
  final int sec = parseTwo(s, 6);
  final int tmp = (h * 60 + m) * 60 + sec;
  return (tmp).toDouble();
}

String pad(int n) {
  if (n < 10) {
    return "0" + (n).toString();
  }
  return (n).toString();
}

String meanTime(List<String> times) {
  num ssum = 0.0;
  num csum = 0.0;
  int i = 0;
  while (i < times.length) {
    final num sec = parseSec(times[i]);
    final num ang = sec * 2.0 * PI / 86400.0;
    ssum = ssum + sinApprox(ang);
    csum = csum + cosApprox(ang);
    i = i + 1;
  }
  num theta = atan2Approx(ssum, csum);
  num frac = theta / 2.0 * PI;
  while (frac < 0.0) {
    frac = frac + 1.0;
  }
  final num total = frac * 86400.0;
  final int si = (total).toInt();
  final int h = si ~/ 3600 as int;
  final int m = si % 3600 ~/ 60 as int;
  final int s = si % 60 as int;
  return pad(h) + ":" + pad(m) + ":" + pad(s);
}

void main() {
  final List<String> inputs = ["23:00:17", "23:40:20", "00:12:45", "00:17:19"];
  print(meanTime(inputs));
}

void _start() {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _initNow();
  {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  main();
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  main();
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "_start"}));
}
