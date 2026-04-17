import 'dart:io';

bool matchAt(String s, String p, int i, int j) {
  if (j == p.length) return i == s.length;
  bool first = i < s.length && (p[j] == '.' || s[i] == p[j]);
  if (j + 1 < p.length && p[j + 1] == '*') {
    return matchAt(s, p, i, j + 2) || (first && matchAt(s, p, i + 1, j));
  }
  return first && matchAt(s, p, i + 1, j + 1);
}

void main() {
  final lines = File('/dev/stdin').readAsStringSync().split(RegExp(r'\r?\n'));
  if (lines.isEmpty || lines.first.trim().isEmpty) return;
  int t = int.parse(lines[0].trim());
  int idx = 1;
  List<String> out = [];
  for (int tc = 0; tc < t; tc++) {
    String s = idx < lines.length ? lines[idx] : '';
    idx++;
    String p = idx < lines.length ? lines[idx] : '';
    idx++;
    out.add(matchAt(s, p, 0, 0) ? 'true' : 'false');
  }
  stdout.write(out.join('\n'));
}
