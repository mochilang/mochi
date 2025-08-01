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

class Birthday {
  int month;
  int day;
  Birthday({required this.month, required this.day});
}

bool monthUnique(Birthday b, List<Birthday> list) {
  int c = 0;
  for (Birthday x in list) {
    if (x.month == b.month) {
    c = c + 1;
  }
  }
  return c == 1;
}

bool dayUnique(Birthday b, List<Birthday> list) {
  int c = 0;
  for (Birthday x in list) {
    if (x.day == b.day) {
    c = c + 1;
  }
  }
  return c == 1;
}

bool monthWithUniqueDay(Birthday b, List<Birthday> list) {
  for (Birthday x in list) {
    if (x.month == b.month && dayUnique(x, list)) {
    return true;
  }
  }
  return false;
}

String bstr(Birthday b) {
  List<String> months = ["", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
  return months[b.month] + " " + (b.day).toString();
}

List<Birthday> choices = [Birthday(month: 5, day: 15), Birthday(month: 5, day: 16), Birthday(month: 5, day: 19), Birthday(month: 6, day: 17), Birthday(month: 6, day: 18), Birthday(month: 7, day: 14), Birthday(month: 7, day: 16), Birthday(month: 8, day: 14), Birthday(month: 8, day: 15), Birthday(month: 8, day: 17)];
List<Birthday> filtered = <Birthday>[];
List<Birthday> filtered2 = <Birthday>[];
List<Birthday> filtered3 = <Birthday>[];
List<Birthday> filtered4 = <Birthday>[];
void main() {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  _initNow();
  {
  var _benchMem0 = ProcessInfo.currentRss;
  var _benchSw = Stopwatch()..start();
  for (Birthday bd in choices) {
    if (!monthUnique(bd, choices)) {
    filtered = [...filtered, bd];
  }
  }
  for (Birthday bd in filtered) {
    if (!monthWithUniqueDay(bd, filtered)) {
    filtered2 = [...filtered2, bd];
  }
  }
  for (Birthday bd in filtered2) {
    if (dayUnique(bd, filtered2)) {
    filtered3 = [...filtered3, bd];
  }
  }
  for (Birthday bd in filtered3) {
    if (monthUnique(bd, filtered3)) {
    filtered4 = [...filtered4, bd];
  }
  }
  if (filtered4.length == 1) {
    print("Cheryl's birthday is " + bstr(filtered4[0]));
  } else {
    print("Something went wrong!");
  }
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
  _benchSw.stop();
  var _benchMem1 = ProcessInfo.currentRss;
  print(jsonEncode({"duration_us": _benchSw.elapsedMicroseconds, "memory_bytes": (_benchMem1 - _benchMem0).abs(), "name": "main"}));
}
