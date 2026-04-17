import 'dart:io';
void main() {
  final lines = File('/dev/stdin').readAsStringSync().split(RegExp(r'\r?\n'));
  if (lines.isEmpty || lines.first.trim().isEmpty) return;
  int idx = 0, t = int.parse(lines[idx++].trim());
  List<String> out = [];
  for (int tc = 0; tc < t; tc++) {
    int k = idx < lines.length ? int.parse(lines[idx++].trim()) : 0;
    List<int> vals = [];
    for (int i = 0; i < k; i++) {
      int n = idx < lines.length ? int.parse(lines[idx++].trim()) : 0;
      for (int j = 0; j < n; j++) { vals.add(idx < lines.length ? int.parse(lines[idx++].trim()) : 0); }
    }
    vals.sort();
    out.add('[' + vals.join(',') + ']');
  }
  stdout.write(out.join('\n'));
}
