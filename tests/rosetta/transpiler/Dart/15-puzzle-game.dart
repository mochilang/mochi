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

class MoveResult {
  int idx;
  bool ok;
  MoveResult({required this.idx, required this.ok});
}

List<int> board = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0];
List<int> solved = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0];
int empty = 15;
int moves = 0;
bool quit = false;
int randMove() {
  return _now() % 4;
}

bool isSolved() {
  dynamic i = 0;
  while (i < 16) {
    if (board[(i).toInt()] != solved[(i).toInt()]) {
    return false;
  }
    i = i + 1;
  }
  return true;
}

MoveResult isValidMove(int m) {
  if (m == 0) {
    return MoveResult(idx: empty - 4, ok: empty ~/ 4 > 0);
  }
  if (m == 1) {
    return MoveResult(idx: empty + 4, ok: empty ~/ 4 < 3);
  }
  if (m == 2) {
    return MoveResult(idx: empty + 1, ok: empty % 4 < 3);
  }
  if (m == 3) {
    return MoveResult(idx: empty - 1, ok: empty % 4 > 0);
  }
  return MoveResult(idx: 0, ok: false);
}

bool doMove(int m) {
  MoveResult r = isValidMove(m);
  if (!r.ok) {
    return false;
  }
  int i = empty;
  int j = r.idx;
  int tmp = board[i];
  board[i] = board[j];
  board[j] = tmp;
  empty = j;
  moves = moves + 1;
  return true;
}

void shuffle(int n) {
  dynamic i = 0;
  while (i < n || isSolved()) {
    if (doMove(randMove())) {
    i = i + 1;
  }
  }
}

void printBoard() {
  dynamic line = "";
  dynamic i = 0;
  while (i < 16) {
    int val = board[(i).toInt()];
    if (val == 0) {
    line = line + "  .";
  } else {
    String s = (val).toString();
    if (val < 10) {
    line = line + "  " + s;
  } else {
    line = line + " " + s;
  };
  }
    if (i % 4 == 3) {
    print(line);
    line = "";
  }
    i = i + 1;
  }
}

void playOneMove() {
  while (true) {
    print("Enter move #" + (moves + 1).toString() + " (U, D, L, R, or Q): ");
    String s = stdin.readLineSync() ?? '';
    if (s == "") {
    continue;
  }
    String c = _substr(s, 0, 1);
    dynamic m = 0;
    if (c == "U" || c == "u") {
    m = 0;
  } else {
    if (c == "D" || c == "d") {
    m = 1;
  } else {
    if (c == "R" || c == "r") {
    m = 2;
  } else {
    if (c == "L" || c == "l") {
    m = 3;
  } else {
    if (c == "Q" || c == "q") {
    print("Quiting after " + (moves).toString() + " moves.");
    quit = true;
    return;
  } else {
    print("Please enter \"U\", \"D\", \"L\", or \"R\" to move the empty cell\n" + "up, down, left, or right. You can also enter \"Q\" to quit.\n" + "Upper or lowercase is accepted and only the first non-blank\n" + "character is important (i.e. you may enter \"up\" if you like).");
    continue;
  };
  };
  };
  };
  }
    if (!doMove(m)) {
    print("That is not a valid move at the moment.");
    continue;
  }
    return;
  }
}

void play() {
  print("Starting board:");
  while (!quit && isSolved() == false) {
    print("");
    printBoard();
    playOneMove();
  }
  if (isSolved()) {
    print("You solved the puzzle in " + (moves).toString() + " moves.");
  }
}

void _main() {
  shuffle(50);
  play();
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
