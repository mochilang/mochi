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

List<String> dayNames = ["Sweetmorn", "Boomtime", "Pungenday", "Prickle-Prickle", "Setting Orange"];
List<String> seasons = ["Chaos", "Discord", "Confusion", "Bureaucracy", "The Aftermath"];
List<List<String>> holydays = [["Mungday", "Chaoflux"], ["Mojoday", "Discoflux"], ["Syaday", "Confuflux"], ["Zaraday", "Bureflux"], ["Maladay", "Afflux"]];
bool isLeap(int y) {
  if (y % 400 == 0) {
    return true;
  }
  if (y % 100 == 0) {
    return false;
  }
  return y % 4 == 0;
}

List<int> daysBefore = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
int dayOfYear(int y, int m, int d) {
  int doy = daysBefore[m - 1] + d;
  if (m > 2 && isLeap(y)) {
    doy = doy + 1;
  }
  return doy;
}

String ordinal(int n) {
  String suff = "th";
  int mod100 = n % 100;
  if (mod100 < 11 || mod100 > 13) {
    int r = n % 10;
    if (r == 1) {
    suff = "st";
  } else {
    if (r == 2) {
    suff = "nd";
  } else {
    if (r == 3) {
    suff = "rd";
  };
  };
  };
  }
  return (n).toString() + suff;
}

String discordian(int y, int m, int d) {
  if (isLeap(y) && m == 2 && d == 29) {
    return "St. Tib's Day, YOLD " + (y + 1166).toString();
  }
  int doy = dayOfYear(y, m, d);
  if (isLeap(y) && doy > 60) {
    doy = doy - 1;
  }
  int idx = doy - 1;
  int season = idx ~/ 73;
  int day = idx % 73;
  String res = dayNames[idx % 5] + ", the " + ordinal(day + 1) + " day of " + seasons[season] + " in the YOLD " + (y + 1166).toString();
  if (day == 4) {
    res = res + ". Celebrate " + holydays[season][0] + "!";
  }
  if (day == 49) {
    res = res + ". Celebrate " + holydays[season][1] + "!";
  }
  return res;
}

void _main() {
  List<List<int>> dates = [[2010, 7, 22], [2012, 2, 28], [2012, 2, 29], [2012, 3, 1], [2012, 12, 31], [2013, 1, 1], [2100, 12, 31], [2015, 10, 19], [2010, 1, 5], [2011, 5, 3], [2000, 3, 13]];
  int i = 0;
  while (i < dates.length) {
    List<int> dt = dates[i];
    print(discordian(dt[0], dt[1], dt[2]));
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
