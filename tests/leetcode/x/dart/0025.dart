import 'dart:io';
void main() {
  final lines = File('/dev/stdin').readAsStringSync().split(RegExp(r'\r?\n'));
  if (lines.isEmpty || lines.first.trim().isEmpty) return;
  int idx = 0, t = int.parse(lines[idx++].trim());
  List<String> out = [];
  for (int tc = 0; tc < t; tc++) {
    int n = idx < lines.length ? int.parse(lines[idx++].trim()) : 0;
    List<int> arr = List.generate(n, (_) => idx < lines.length ? int.parse(lines[idx++].trim()) : 0);
    int k = idx < lines.length ? int.parse(lines[idx++].trim()) : 1;
    for (int i = 0; i + k <= arr.length; i += k) {
      for (int l = i, r = i + k - 1; l < r; l++, r--) {
        int tmp = arr[l]; arr[l] = arr[r]; arr[r] = tmp;
      }
    }
    out.add('[' + arr.join(',') + ']');
  }
  stdout.write(out.join('\n'));
}
