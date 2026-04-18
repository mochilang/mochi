import 'dart:convert';
import 'dart:io';
int trap(List<int> h) {
  var left = 0, right = h.length - 1, leftMax = 0, rightMax = 0, water = 0;
  while (left <= right) {
    if (leftMax <= rightMax) {
      if (h[left] < leftMax) water += leftMax - h[left]; else leftMax = h[left];
      left++;
    } else {
      if (h[right] < rightMax) water += rightMax - h[right]; else rightMax = h[right];
      right--;
    }
  }
  return water;
}
Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty || lines[0].trim().isEmpty) return;
  var idx = 0;
  final t = int.parse(lines[idx++].trim());
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(lines[idx++].trim());
    final arr = <int>[];
    for (var i = 0; i < n; i++) arr.add(int.parse(lines[idx++].trim()));
    out.add(trap(arr).toString());
  }
  stdout.write(out.join('\n'));
}
