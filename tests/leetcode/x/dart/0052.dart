import 'dart:convert';
import 'dart:io';
List<List<String>> solve(int n) {
  final cols = List<bool>.filled(n, false);
  final d1 = List<bool>.filled(2 * n, false);
  final d2 = List<bool>.filled(2 * n, false);
  final board = List.generate(n, (_) => List<String>.filled(n, '.'));
  final res = <List<String>>[];
  void dfs(int r) {
    if (r == n) { res.add(board.map((row) => row.join()).toList()); return; }
    for (var c = 0; c < n; c++) {
      final a = r + c, b = r - c + n - 1;
      if (cols[c] || d1[a] || d2[b]) continue;
      cols[c] = d1[a] = d2[b] = true; board[r][c] = 'Q'; dfs(r + 1); board[r][c] = '.'; cols[c] = d1[a] = d2[b] = false;
    }
  }
  dfs(0); return res;
}
Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty || lines[0].trim().isEmpty) return;
  var idx = 0; final t = int.parse(lines[idx++].trim()); final out = <String>[];
  for (var tc = 0; tc < t; tc++) { final n = int.parse(lines[idx++].trim()); final sols = solve(n); out.add(sols.length.toString()); }
  stdout.write(out.join('\n'));
}
