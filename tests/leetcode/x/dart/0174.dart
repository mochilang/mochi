import 'dart:io';

int solve(List<List<int>> dungeon) {
  final cols = dungeon[0].length;
  final dp = List<int>.filled(cols + 1, 1 << 60);
  dp[cols - 1] = 1;
  for (var i = dungeon.length - 1; i >= 0; i--) {
    for (var j = cols - 1; j >= 0; j--) {
      final need = (dp[j] < dp[j + 1] ? dp[j] : dp[j + 1]) - dungeon[i][j];
      dp[j] = need <= 1 ? 1 : need;
    }
  }
  return dp[0];
}

void main() {
  final toks = <String>[];
  String? line;
  while ((line = stdin.readLineSync()) != null) {
    for (final p in line!.trim().split(RegExp(r'\s+'))) {
      if (p.isNotEmpty) toks.add(p);
    }
  }
  if (toks.isEmpty) return;
  var idx = 0;
  final t = int.parse(toks[idx++]);
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final rows = int.parse(toks[idx++]);
    final cols = int.parse(toks[idx++]);
    final dungeon = <List<int>>[];
    for (var i = 0; i < rows; i++) {
      final row = <int>[];
      for (var j = 0; j < cols; j++) row.add(int.parse(toks[idx++]));
      dungeon.add(row);
    }
    out.add('${solve(dungeon)}');
  }
  stdout.write(out.join('\n'));
}
