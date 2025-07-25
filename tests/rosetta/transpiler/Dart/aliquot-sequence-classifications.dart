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
  final int THRESHOLD = 140737488355328;
  int indexOf(List<int> xs, int value) {
  int i = 0;
  while (i < xs.length) {
    if (xs[i] == value) {
    return i;
  }
    i = i + 1;
  }
  return 0 - 1;
}
  bool contains(List<int> xs, int value) {
  return xs.indexOf(value) != 0 - 1;
}
  int maxOf(int a, int b) {
  if (a > b) {
    return a;
  } else {
    return b;
  }
}
  int intSqrt(int n) {
  if (n == 0) {
    return 0;
  }
  int x = n;
  int y = (x + 1) ~/ 2;
  while (y < x) {
    x = y;
    y = (x + n ~/ x) ~/ 2;
  }
  return x;
}
  int sumProperDivisors(int n) {
  if (n < 2) {
    return 0;
  }
  final int sqrt = intSqrt(n);
  int sum = 1;
  int i = 2;
  while (i <= sqrt) {
    if (n % i == 0) {
    sum = sum + i + n ~/ i;
  }
    i = i + 1;
  }
  if (sqrt * sqrt == n) {
    sum = sum - sqrt;
  }
  return sum;
}
  Map<String, dynamic> classifySequence(int k) {
  int last = k;
  List<int> seq = [k];
  while (true) {
    last = sumProperDivisors(last);
    seq = [...seq, last];
    final int n = seq.length;
    String aliquot = "";
    if (last == 0) {
    aliquot = "Terminating";
  } else {
    if (n == 2 && last == k) {
    aliquot = "Perfect";
  } else {
    if (n == 3 && last == k) {
    aliquot = "Amicable";
  } else {
    if (n >= 4 && last == k) {
    aliquot = "Sociable[" + (n - 1).toString() + "]";
  } else {
    if (last == seq[n - 2]) {
    aliquot = "Aspiring";
  } else {
    if (contains(seq.sublist(1, maxOf(1, n - 2)), last)) {
    final int idx = seq.indexOf(last);
    aliquot = "Cyclic[" + (n - 1 - idx).toString() + "]";
  } else {
    if (n == 16 || last > THRESHOLD) {
    aliquot = "Non-Terminating";
  };
  };
  };
  };
  };
  };
  }
    if (aliquot != "") {
    return {"seq": seq, "aliquot": aliquot};
  }
  }
  return {"seq": seq, "aliquot": ""};
}
  String padLeft(int n, int w) {
  String s = (n).toString();
  while (s.length < w) {
    s = " " + s;
  }
  return s;
}
  String padRight(String s, int w) {
  String r = s;
  while (r.length < w) {
    r = r + " ";
  }
  return r;
}
  String joinWithCommas(List<int> seq) {
  String s = "[";
  int i = 0;
  while (i < seq.length) {
    s = s + (seq[i]).toString();
    if (i < seq.length - 1) {
    s = s + ", ";
  }
    i = i + 1;
  }
  s = s + "]";
  return s;
}
  void main() {
  print("Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n");
  int k = 1;
  while (k <= 10) {
    final Map<String, dynamic> res = classifySequence(k);
    print(padLeft(k, 2) + ": " + padRight(res["aliquot"]!! as String, 15) + " " + joinWithCommas(res["seq"]!! as List<int>));
    k = k + 1;
  }
  print("");
  final List<int> s = [11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488];
  int i = 0;
  while (i < s.length) {
    final int val = s[i];
    final Map<String, dynamic> res = classifySequence(val);
    print(padLeft(val, 7) + ": " + padRight(res["aliquot"]!! as String, 15) + " " + joinWithCommas(res["seq"]!! as List<int>));
    i = i + 1;
  }
  print("");
  final int big = 15355717786080;
  final Map<String, dynamic> r = classifySequence(big);
  print((big).toString() + ": " + padRight(r["aliquot"]!! as String, 15) + " " + joinWithCommas(r["seq"]!! as List<int>));
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
