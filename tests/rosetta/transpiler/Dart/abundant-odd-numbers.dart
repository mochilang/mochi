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
  List<int> divisors(int n) {
  List<int> divs = [1];
  List<int> divs2 = [];
  int i = 2;
  while (i * i <= n) {
    if (n % i == 0) {
    final int j = n ~/ i as int;
    divs = [...divs, i];
    if (i != j) {
    divs2 = [...divs2, j];
  };
  }
    i = i + 1;
  }
  int j = divs2.length - 1;
  while (j >= 0) {
    divs = [...divs, divs2[j]];
    j = j - 1;
  }
  return divs;
}
  int sum(List<int> xs) {
  int tot = 0;
  for (var v in xs) {
    tot = tot + v;
  }
  return tot;
}
  String sumStr(List<int> xs) {
  String s = "";
  int i = 0;
  while (i < xs.length) {
    s = s + (xs[i]).toString() + " + ";
    i = i + 1;
  }
  return s.substring(0, s.length - 3);
}
  String pad2(int n) {
  final String s = (n).toString();
  if (s.length < 2) {
    return " " + s;
  }
  return s;
}
  String pad5(int n) {
  String s = (n).toString();
  while (s.length < 5) {
    s = " " + s;
  }
  return s;
}
  int abundantOdd(int searchFrom, int countFrom, int countTo, bool printOne) {
  int count = countFrom;
  int n = searchFrom;
  while (count < countTo) {
    final List<int> divs = divisors(n);
    final num tot = divs.fold(0, (a, b) => a + b);
    if (tot > n) {
    count = count + 1;
    if (printOne && count < countTo) {
    n = n + 2;
    continue;
  };
    final String s = sumStr(divs);
    if (!printOne) {
    print(pad2(count) + ". " + pad5(n) + " < " + s + " = " + (tot).toString());
  } else {
    print((n).toString() + " < " + s + " = " + (tot).toString());
  };
  }
    n = n + 2;
  }
  return n;
}
  void main() {
  final int max = 25;
  print("The first " + (max).toString() + " abundant odd numbers are:");
  final int n = abundantOdd(1, 0, max, false);
  print("\nThe one thousandth abundant odd number is:");
  abundantOdd(n, max, 1000, true);
  print("\nThe first abundant odd number above one billion is:");
  abundantOdd(1000000001, 0, 1, true);
}
  main();
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
