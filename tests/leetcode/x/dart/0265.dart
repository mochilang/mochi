import 'dart:io';

int solve(List<List<int>> costs) {
  if (costs.isEmpty) return 0;
  var prev = List<int>.from(costs[0]);
  for (var r = 1; r < costs.length; r++) {
    var min1 = 1 << 60;
    var min2 = 1 << 60;
    var idx1 = -1;
    for (var i = 0; i < prev.length; i++) {
      final v = prev[i];
      if (v < min1) {
        min2 = min1;
        min1 = v;
        idx1 = i;
      } else if (v < min2) {
        min2 = v;
      }
    }
    final cur = List<int>.filled(prev.length, 0);
    for (var i = 0; i < prev.length; i++) {
      cur[i] = costs[r][i] + (i == idx1 ? min2 : min1);
    }
    prev = cur;
  }
  return prev.reduce((a, b) => a < b ? a : b);
}

void main() {
  final toks = File('/dev/stdin').readAsStringSync().trim().split(RegExp(r'\s+')).where((s) => s.isNotEmpty).toList();
  if (toks.isEmpty) return;
  var idx = 0;
  final t = int.parse(toks[idx++]);
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(toks[idx++]);
    final k = int.parse(toks[idx++]);
    final costs = List.generate(n, (_) => List<int>.filled(k, 0));
    for (var i = 0; i < n; i++) {
      for (var j = 0; j < k; j++) costs[i][j] = int.parse(toks[idx++]);
    }
    out.add(solve(costs).toString());
  }
  stdout.write(out.join('\n'));
}
