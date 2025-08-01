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

String join(List<String> xs, String sep) {
  String res = "";
  int i = 0;
  while (i < xs.length) {
    if (i > 0) {
    res = res + sep;
  }
    res = res + xs[i];
    i = i + 1;
  }
  return res;
}

List<Map<String, dynamic>> sortPairs(List<Map<String, dynamic>> xs) {
  List<Map<String, dynamic>> arr = xs;
  int i = 1;
  while (i < arr.length) {
    int j = i;
    while (j > 0 && (arr[j - 1]["count"]! as int) < (arr[j]["count"]! as int)) {
    Map<String, dynamic> tmp = arr[j - 1];
    arr[j - 1] = arr[j];
    arr[j] = tmp;
    j = j - 1;
  }
    i = i + 1;
  }
  return arr;
}

bool isAlphaNumDot(String ch) {
  return ch.compareTo("A") >= 0 && ch.compareTo("Z") <= 0 || ch.compareTo("a") >= 0 && ch.compareTo("z") <= 0 || ch.compareTo("0") >= 0 && ch.compareTo("9") <= 0 || ch == "_" || ch == ".";
}

void _main() {
  List<String> srcLines = ["package main", "", "import (", "    \"fmt\"", "    \"go/ast\"", "    \"go/parser\"", "    \"go/token\"", "    \"io/ioutil\"", "    \"os\"", "    \"sort\"", ")", "", "func main() {", "    if len(os.Args) != 2 {", "        fmt.Println(\"usage ff <go source filename>\")", "        return", "    }", "    src, err := ioutil.ReadFile(os.Args[1])", "    if err != nil {", "        fmt.Println(err)", "        return", "    }", "    fs := token.NewFileSet()", "    a, err := parser.ParseFile(fs, os.Args[1], src, 0)", "    if err != nil {", "        fmt.Println(err)", "        return", "    }", "    f := fs.File(a.Pos())", "    m := make(map[string]int)", "    ast.Inspect(a, func(n ast.Node) bool {", "        if ce, ok := n.(*ast.CallExpr); ok {", "            start := f.Offset(ce.Pos())", "            end := f.Offset(ce.Lparen)", "            m[string(src[start:end])]++", "        }", "        return true", "    })", "    cs := make(calls, 0, len(m))", "    for k, v := range m {", "        cs = append(cs, &call{k, v})", "    }", "    sort.Sort(cs)", "    for i, c := range cs {", "        fmt.Printf(\"%-20s %4d\\n\", c.expr, c.count)", "        if i == 9 {", "            break", "        }", "    }", "}", "", "type call struct {", "    expr  string", "    count int", "}", "type calls []*call", "", "func (c calls) Len() int           { return len(c) }", "func (c calls) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }", "func (c calls) Less(i, j int) bool { return c[i].count > c[j].count }"];
  String src = join(srcLines, "\n");
  Map<String, int> freq = <String, int>{};
  int i = 0;
  List<String> order = <String>[];
  while (i < src.length) {
    String ch = _substr(src, i, i + 1);
    if (ch.compareTo("A") >= 0 && ch.compareTo("Z") <= 0 || ch.compareTo("a") >= 0 && ch.compareTo("z") <= 0 || ch == "_") {
    int j = i + 1;
    while (j < src.length && isAlphaNumDot(_substr(src, j, j + 1))) {
    j = j + 1;
  };
    String token = _substr(src, i, j);
    int k = j;
    while (k < src.length) {
    String cc = _substr(src, k, k + 1);
    if (cc == " " || cc == "	" || cc == "\n" || cc == "\r") {
    k = k + 1;
  } else {
    break;
  }
  };
    if (k < src.length && _substr(src, k, k + 1) == "(") {
    int p = i - 1;
    while (p >= 0 && (_substr(src, p, p + 1) == " " || _substr(src, p, p + 1) == "	")) {
    p = p - 1;
  };
    bool skip = false;
    if (p >= 3) {
    String before = _substr(src, p - 3, p + 1);
    if (before == "func") {
    skip = true;
  };
  };
    if (!skip) {
    if (freq.containsKey(token)) {
    freq[token] = freq[token]! + 1;
  } else {
    freq[token] = 1;
    order = [...order, token];
  };
  };
  };
    i = j;
  } else {
    i = i + 1;
  }
  }
  List<Map<String, dynamic>> pairs = <Map<String, dynamic>>[];
  for (String t in order) {
    pairs = [...pairs, {"expr": t, "count": freq[t]!}];
  }
  pairs = sortPairs(pairs);
  int idx = 0;
  while (idx < pairs.length && idx < 10) {
    Map<String, dynamic> p = pairs[idx];
    print(p["expr"]! + " " + (p["count"]!).toString());
    idx = idx + 1;
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
