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

int parseIntStr(String str) {
  dynamic i = 0;
  dynamic neg = false;
  if (str.length > 0 && _substr(str, 0, 1) == "-") {
    neg = true;
    i = 1;
  }
  dynamic n = 0;
  Map<String, int> digits = {"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9};
  while (i < str.length) {
    n = n * 10 + (digits[_substr(str, i, i + 1)] ?? 0);
    i = i + 1;
  }
  if (neg) {
    n = -n;
  }
  return n;
}

void _main() {
  dynamic total = 0;
  dynamic computer = _now() % 2 == 0;
  print("Enter q to quit at any time\n");
  if (computer) {
    print("The computer will choose first");
  } else {
    print("You will choose first");
  }
  print("\n\nRunning total is now 0\n\n");
  dynamic round = 1;
  dynamic done = false;
  while (!done) {
    print("ROUND " + (round).toString() + ":\n\n");
    dynamic i = 0;
    while (i < 2 && !done) {
    if (computer) {
    dynamic choice = 0;
    if (total < 18) {
    choice = _now() % 3 + 1;
  } else {
    choice = 21 - total;
  };
    total = total + choice;
    print("The computer chooses " + (choice).toString());
    print("Running total is now " + (total).toString());
    if (total == 21) {
    print("\nSo, commiserations, the computer has won!");
    done = true;
  };
  } else {
    while (true) {
    print("Your choice 1 to 3 : ");
    String line = stdin.readLineSync() ?? '';
    if (line == "q" || line == "Q") {
    print("OK, quitting the game");
    done = true;
    break;
  }
    dynamic _num = int.parse(line);
    if (_num < 1 || _num > 3) {
    if (total + _num > 21) {
    print("Too big, try again");
  } else {
    print("Out of range, try again");
  };
    continue;
  }
    if (total + _num > 21) {
    print("Too big, try again");
    continue;
  }
    total = total + _num;
    print("Running total is now " + (total).toString());
    break;
  };
    if (total == 21) {
    print("\nSo, congratulations, you've won!");
    done = true;
  };
  }
    print("\n");
    computer = !computer;
    i = i + 1;
  }
    round = round + 1;
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
