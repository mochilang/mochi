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

int nextPrime(List<int> primes, int start) {
  int n = start;
  while (true) {
    bool isP = true;
    int i = 0;
    while (i < primes.length) {
    int p = primes[i];
    if (p * p > n) {
    break;
  }
    if (n % p == 0) {
    isP = false;
    break;
  }
    i = i + 1;
  }
    if (isP) {
    return n;
  }
    n = n + 2;
  }
}

void _main() {
  List<int> primes = [2];
  int cand = 3;
  while (primes.length < 10000) {
    cand = nextPrime(primes, cand);
    primes = [...primes, cand];
    cand = cand + 2;
  }
  String line = "First twenty:";
  int i = 0;
  while (i < 20) {
    line = line + " " + (primes[i]).toString();
    i = i + 1;
  }
  print(line);
  int idx = 0;
  while (primes[idx] <= 100) {
    idx = idx + 1;
  }
  line = "Between 100 and 150: " + (primes[idx]).toString();
  idx = idx + 1;
  while (primes[idx] < 150) {
    line = line + " " + (primes[idx]).toString();
    idx = idx + 1;
  }
  print(line);
  while (primes[idx] <= 7700) {
    idx = idx + 1;
  }
  int count = 0;
  while (primes[idx] < 8000) {
    count = count + 1;
    idx = idx + 1;
  }
  print("Number beween 7,700 and 8,000: " + (count).toString());
  print("10,000th prime: " + (primes[9999]).toString());
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
