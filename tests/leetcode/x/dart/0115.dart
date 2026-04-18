import 'dart:convert';
import 'dart:io';

int solve(String s, String t) {
  final dp = List.filled(t.length + 1, 0);
  dp[0] = 1;
  for (var i = 0; i < s.length; i++) {
    for (var j = t.length; j >= 1; j--) {
      if (s.codeUnitAt(i) == t.codeUnitAt(j - 1)) dp[j] += dp[j - 1];
    }
  }
  return dp[t.length];
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  final tc = int.parse(lines[0].trim());
  final out = <String>[];
  for (var i = 0; i < tc; i++) {
    out.add(solve(lines[1 + 2 * i], lines[2 + 2 * i]).toString());
  }
  stdout.write(out.join('\n'));
}
