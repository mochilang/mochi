import 'dart:io';

bool isScramble(String s1, String s2) {
  final memo = <String, bool>{};
  bool dfs(int i1, int i2, int len) {
    final key = '$i1,$i2,$len';
    if (memo.containsKey(key)) return memo[key]!;
    final a = s1.substring(i1, i1 + len), b = s2.substring(i2, i2 + len);
    if (a == b) return memo[key] = true;
    final cnt = List.filled(26, 0);
    for (var i = 0; i < len; i++) {
      cnt[a.codeUnitAt(i) - 97]++;
      cnt[b.codeUnitAt(i) - 97]--;
    }
    if (cnt.any((v) => v != 0)) return memo[key] = false;
    for (var k = 1; k < len; k++) {
      if ((dfs(i1, i2, k) && dfs(i1 + k, i2 + k, len - k)) ||
          (dfs(i1, i2 + len - k, k) && dfs(i1 + k, i2, len - k))) return memo[key] = true;
    }
    return memo[key] = false;
  }
  return dfs(0, 0, s1.length);
}

void main() {
  final lines = <String>[];
  String? line;
  while ((line = stdin.readLineSync()) != null) lines.add(line!.replaceAll('\r', ''));
  if (lines.isEmpty || lines[0].trim().isEmpty) return;
  final t = int.parse(lines[0].trim());
  final out = <String>[];
  for (var i = 0; i < t; i++) out.add(isScramble(lines[1 + 2 * i], lines[2 + 2 * i]) ? 'true' : 'false');
  stdout.write(out.join('\n'));
}
