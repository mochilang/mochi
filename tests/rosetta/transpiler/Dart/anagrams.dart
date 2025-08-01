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
  String sortRunes(String s) {
  List<String> arr = [];
  int i = 0;
  while (i < s.length) {
    arr = [...arr, s.substring(i, i + 1)];
    i = i + 1;
  }
  int n = arr.length;
  int m = 0;
  while (m < n) {
    int j = 0;
    while (j < n - 1) {
    if (arr[j].compareTo(arr[j + 1]) > 0) {
    final String tmp = arr[j];
    arr[j] = arr[j + 1];
    arr[j + 1] = tmp;
  }
    j = j + 1;
  }
    m = m + 1;
  }
  String out = "";
  i = 0;
  while (i < n) {
    out = out + arr[i];
    i = i + 1;
  }
  return out;
}
  List<String> sortStrings(List<String> xs) {
  List<String> res = [];
  List<String> tmp = xs;
  while (tmp.length > 0) {
    String min = tmp[0];
    int idx = 0;
    int i = 1;
    while (i < tmp.length) {
    if (tmp[i].compareTo(min) < 0) {
    min = tmp[i];
    idx = i;
  }
    i = i + 1;
  }
    res = [...res, min];
    List<String> out = [];
    int j = 0;
    while (j < tmp.length) {
    if (j != idx) {
    out = [...out, tmp[j]];
  }
    j = j + 1;
  }
    tmp = out;
  }
  return res;
}
  void main() {
  final List<String> words = ["abel", "able", "bale", "bela", "elba", "alger", "glare", "lager", "large", "regal", "angel", "angle", "galen", "glean", "lange", "caret", "carte", "cater", "crate", "trace", "elan", "lane", "lean", "lena", "neal", "evil", "levi", "live", "veil", "vile"];
  Map<String, List<String>> groups = {};
  int maxLen = 0;
  for (var w in words) {
    final String k = sortRunes(w);
    if (!groups.containsKey(k)) {
    groups[k] = [w];
  } else {
    groups[k] = [...groups[k]!, w];
  }
    if (groups[k]!.length > maxLen) {
    maxLen = groups[k]!.length;
  }
  }
  Map<String, bool> printed = {};
  for (var w in words) {
    final String k = sortRunes(w);
    if (groups[k]!.length == maxLen) {
    if (!printed.containsKey(k)) {
    List<String> g = sortStrings(groups[k]!);
    String line = "[" + g[0];
    int i = 1;
    while (i < g.length) {
    line = line + " " + g[i];
    i = i + 1;
  };
    line = line + "]";
    print(line);
    printed[k] = true;
  };
  }
  }
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
