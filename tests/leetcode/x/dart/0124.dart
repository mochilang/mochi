import 'dart:convert';
import 'dart:io';

int solve(List<int> vals, List<bool> ok) {
  var best = -1000000000;
  int dfs(int i) {
    if (i >= vals.length || !ok[i]) return 0;
    final left = (dfs(2 * i + 1));
    final right = (dfs(2 * i + 2));
    final lg = left > 0 ? left : 0;
    final rg = right > 0 ? right : 0;
    final total = vals[i] + lg + rg;
    if (total > best) best = total;
    return vals[i] + (lg > rg ? lg : rg);
  }
  dfs(0);
  return best;
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  final tc = int.parse(lines[0].trim());
  var idx = 1;
  final out = <String>[];
  for (var t = 0; t < tc; t++) {
    final n = int.parse(lines[idx++].trim());
    final vals = List.filled(n, 0);
    final ok = List.filled(n, false);
    for (var i = 0; i < n; i++) {
      final tok = lines[idx++].trim();
      if (tok != 'null') { ok[i] = true; vals[i] = int.parse(tok); }
    }
    out.add(solve(vals, ok).toString());
  }
  stdout.write(out.join('\n'));
}
