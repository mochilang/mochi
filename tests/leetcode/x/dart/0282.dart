import 'dart:io';

List<String> solve(String num, int target) {
  final ans = <String>[];
  void dfs(int i, String expr, int value, int last) {
    if (i == num.length) {
      if (value == target) ans.add(expr);
      return;
    }
    for (var j = i; j < num.length; j++) {
      if (j > i && num[i] == '0') break;
      final s = num.substring(i, j + 1);
      final n = int.parse(s);
      if (i == 0) {
        dfs(j + 1, s, n, n);
      } else {
        dfs(j + 1, '$expr+$s', value + n, n);
        dfs(j + 1, '$expr-$s', value - n, -n);
        dfs(j + 1, '$expr*$s', value - last + last * n, last * n);
      }
    }
  }
  dfs(0, '', 0, 0);
  ans.sort();
  return ans;
}

void main() {
  final lines = File('/dev/stdin').readAsLinesSync();
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  final blocks = <String>[];
  var idx = 1;
  for (var tc = 0; tc < t; tc++) {
    final num = lines[idx++].trim();
    final target = int.parse(lines[idx++].trim());
    final ans = solve(num, target);
    blocks.add(([ans.length.toString()] + ans).join('\n'));
  }
  stdout.write(blocks.join('\n\n'));
}
