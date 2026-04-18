import 'dart:io';

int solve(List<List<int>> tri) {
  final dp = List<int>.from(tri.last);
  for (var i = tri.length - 2; i >= 0; i--) {
    for (var j = 0; j <= i; j++) dp[j] = tri[i][j] + (dp[j] < dp[j + 1] ? dp[j] : dp[j + 1]);
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
    final tri = <List<int>>[];
    for (var r = 1; r <= rows; r++) {
      final row = <int>[];
      for (var j = 0; j < r; j++) row.add(int.parse(toks[idx++]));
      tri.add(row);
    }
    out.add('${solve(tri)}');
  }
  stdout.write(out.join('\n'));
}
