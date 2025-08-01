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

class Fps {
  List<num> coeffs;
  dynamic compute;
  Fps({required this.coeffs, required this.compute});
}

class Pair {
  Fps sin;
  Fps cos;
  Pair({required this.sin, required this.cos});
}

Fps newFps(dynamic fn) {
  return Fps(coeffs: [], compute: fn);
}

num extract(Fps f, int n) {
  while (f.coeffs.length <= n) {
    int idx = f.coeffs.length;
    var v = f.compute(idx);
    f.coeffs = [...f.coeffs, v];
  }
  return f.coeffs[n];
}

Fps one() {
  return newFps(((i) {
  if (i == 0) {
    return 1.0;
  };
  return 0.0;
}));
}

Fps add(Fps a, Fps b) {
  return newFps(((n) => extract(a, n) + extract(b, n)));
}

Fps sub(Fps a, Fps b) {
  return newFps(((n) => extract(a, n) - extract(b, n)));
}

Fps mul(Fps a, Fps b) {
  return newFps(((n) {
  num s = 0.0;
  int k = 0;
  while (k.toString().compareTo(n.toString()) <= 0) {
    s = s + extract(a, k) * extract(b, (n - k).toInt());
    k = k + 1;
  };
  return s;
}));
}

Fps div(Fps a, Fps b) {
  Fps q = newFps(((n) => 0.0));
  q.compute = ((n) {
  num b0 = extract(b, 0);
  if (b0 == 0.0) {
    return 0.0 / 0.0;
  };
  num s = extract(a, n);
  int k = 1;
  while (k.toString().compareTo(n.toString()) <= 0) {
    s = s - extract(b, k) * extract(q, (n - k).toInt());
    k = k + 1;
  };
  return s / b0;
});
  return q;
}

Fps differentiate(Fps a) {
  return newFps(((n) => (n + 1 as num) * extract(a, (n + 1).toInt())));
}

Fps integrate(Fps a) {
  return newFps(((n) {
  if (n == 0) {
    return 0.0;
  };
  return extract(a, (n - 1).toInt()) / (n as num);
}));
}

Pair sinCos() {
  Fps sin = newFps(((n) => 0.0));
  Fps cos = sub(one(), integrate(sin));
  sin.compute = ((n) {
  if (n == 0) {
    return 0.0;
  };
  return extract(cos, (n - 1).toInt()) / (n as num);
});
  return Pair(sin: sin, cos: cos);
}

num floorf(num x) {
  int y = (x).toInt();
  return (y).toDouble();
}

String fmtF5(num x) {
  num y = floorf(x * 100000.0 + 0.5) / 100000.0;
  String s = (y).toString();
  int dot = s.indexOf(".");
  if (dot == 0 - 1) {
    s = s + ".00000";
  } else {
    int decs = s.length - dot - 1;
    if (decs > 5) {
    s = _substr(s, 0, dot + 6);
  } else {
    while (decs < 5) {
    s = s + "0";
    decs = decs + 1;
  };
  };
  }
  return s;
}

String padFloat5(num x, int width) {
  String s = fmtF5(x);
  while (s.length < width) {
    s = " " + s;
  }
  return s;
}

String partialSeries(Fps f) {
  String out = "";
  int i = 0;
  while (i < 6) {
    out = out + " " + padFloat5(extract(f, i), 8) + " ";
    i = i + 1;
  }
  return out;
}

void _main() {
  Pair p = sinCos();
  print("sin:" + partialSeries(p.sin));
  print("cos:" + partialSeries(p.cos));
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
