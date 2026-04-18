import 'dart:io';

bool solve(String s1, String s2, String s3) {
  final m = s1.length, n = s2.length;
  if (m + n != s3.length) return false;
  final dp = List.generate(m + 1, (_) => List.filled(n + 1, false));
  dp[0][0] = true;
  for (var i = 0; i <= m; i++) {
    for (var j = 0; j <= n; j++) {
      if (i > 0 && dp[i - 1][j] && s1[i - 1] == s3[i + j - 1]) dp[i][j] = true;
      if (j > 0 && dp[i][j - 1] && s2[j - 1] == s3[i + j - 1]) dp[i][j] = true;
    }
  }
  return dp[m][n];
}

void main() {
  final lines = <String>[];
  String? line;
  while ((line = stdin.readLineSync()) != null) lines.add(line!.replaceAll('\r', ''));
  if (lines.isEmpty || lines[0].trim().isEmpty) return;
  final t = int.parse(lines[0].trim());
  final out = <String>[];
  for (var i = 0; i < t; i++) out.add(solve(lines[1 + 3 * i], lines[2 + 3 * i], lines[3 + 3 * i]) ? 'true' : 'false');
  stdout.write(out.join('\n'));
}
