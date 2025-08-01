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

String _substr(String s, num start, num end) {
  var n = s.length;
  int s0 = start.toInt();
  int e0 = end.toInt();
  if (s0 < 0) s0 += n;
  if (e0 < 0) e0 += n;
  if (s0 < 0) s0 = 0;
  if (s0 > n) s0 = n;
  if (e0 < 0) e0 = 0;
  if (e0 > n) e0 = n;
  if (s0 > e0) s0 = e0;
  return s.substring(s0, e0);
}

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

BigInt fermat(int n) {
  int p = pow_int(2, n);
  return pow_big(BigInt.from(2), p) + (BigInt.from(1));
}

List<BigInt> primeFactorsBig(BigInt n) {
  List<BigInt> factors = <BigInt>[];
  BigInt m = n;
  BigInt d = BigInt.from(2);
  while (m % d == BigInt.from(0)) {
    factors = [...factors, d];
    m = m ~/ d;
  }
  d = BigInt.from(3);
  while (d * d <= m) {
    while (m % d == BigInt.from(0)) {
    factors = [...factors, d];
    m = m ~/ d;
  }
    d = d + BigInt.from(2);
  }
  if (m > BigInt.from(1)) {
    factors = [...factors, m];
  }
  return factors;
}

String show_list(List<BigInt> xs) {
  String line = "";
  int i = 0;
  while (i < xs.length) {
    line = line + (xs[i]).toString();
    if (i < xs.length - 1) {
    line = line + " ";
  }
    i = i + 1;
  }
  return line;
}

void _main() {
  List<BigInt> nums = <BigInt>[];
  for (int i = 0; i < 8; i++) {
    nums = [...nums, fermat(i)];
  }
  print("First 8 Fermat numbers:");
  for (BigInt n in nums) {
    print((n).toString());
  }
  Map<int, List<BigInt>> extra = {6: [BigInt.from(274177), BigInt.from(67280421310721)], 7: [parseBigInt("59649589127497217"), parseBigInt("5704689200685129054721")]};
  print("\nFactors:");
  int i = 0;
  while (i < nums.length) {
    List<BigInt> facs = <BigInt>[];
    if (i <= 5) {
    facs = primeFactorsBig(nums[i]);
  } else {
    facs = List<BigInt>.from(extra[i]!);
  }
    print("F" + (i).toString() + " = " + show_list(facs));
    i = i + 1;
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
