import 'dart:convert';
import 'dart:io';

int maxArea(List<int> h) {
  var left = 0, right = h.length - 1, best = 0;
  while (left < right) {
    final height = h[left] < h[right] ? h[left] : h[right];
    final area = (right - left) * height;
    if (area > best) best = area;
    if (h[left] < h[right]) { left++; } else { right--; }
  }
  return best;
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  var idx = 1;
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(lines[idx++].trim());
    final h = <int>[];
    for (var i = 0; i < n; i++) h.add(int.parse(lines[idx++].trim()));
    out.add(maxArea(h).toString());
  }
  stdout.write(out.join('\n'));
}
