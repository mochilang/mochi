import 'dart:convert';
import 'dart:io';

bool isMatch(String s, String p) {
  var i = 0, j = 0, star = -1, match = 0;
  while (i < s.length) {
    if (j < p.length && (p[j] == '?' || p[j] == s[i])) { i++; j++; }
    else if (j < p.length && p[j] == '*') { star = j; match = i; j++; }
    else if (star != -1) { j = star + 1; match++; i = match; }
    else return false;
  }
  while (j < p.length && p[j] == '*') j++;
  return j == p.length;
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty || lines[0].trim().isEmpty) return;
  var idx = 0;
  final t = int.parse(lines[idx++].trim());
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(lines[idx++].trim());
    final s = n > 0 ? lines[idx++] : '';
    final m = int.parse(lines[idx++].trim());
    final p = m > 0 ? lines[idx++] : '';
    out.add(isMatch(s, p) ? 'true' : 'false');
  }
  stdout.write(out.join('\n'));
}
