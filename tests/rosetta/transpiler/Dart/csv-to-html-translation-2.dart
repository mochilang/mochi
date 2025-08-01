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

String c = "Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n" + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" + "The multitude,Who are you?\n" + "Brians mother,I'm his mother; that's who!\n" + "The multitude,Behold his mother! Behold his mother!";
List<List<String>> rows = <List<String>>[];
bool headings = true;
void main() {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _initNow();
  {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  for (var line in c.split("\n")) {
    rows = [...rows, line.split(",")];
  }
  print("<table>");
  if (headings) {
    if (rows.length > 0) {
    String th = "";
    for (String h in rows[0]) {
    th = th + "<th>" + h + "</th>";
  };
    print("   <thead>");
    print("      <tr>" + th + "</tr>");
    print("   </thead>");
    print("   <tbody>");
    int i = 1;
    while (i < rows.length) {
    String cells = "";
    for (String cell in rows[i]) {
    cells = cells + "<td>" + cell + "</td>";
  }
    print("      <tr>" + cells + "</tr>");
    i = i + 1;
  };
    print("   </tbody>");
  };
  } else {
    for (List<String> row in rows) {
    String cells = "";
    for (String cell in row) {
    cells = cells + "<td>" + cell + "</td>";
  }
    print("    <tr>" + cells + "</tr>");
  };
  }
  print("</table>");
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
