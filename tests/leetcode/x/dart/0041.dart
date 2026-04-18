import 'dart:convert';
import 'dart:io';

int firstMissingPositive(List<int> nums) {
  final n = nums.length;
  var i = 0;
  while (i < n) {
    final v = nums[i];
    if (v >= 1 && v <= n && nums[v - 1] != v) {
      final tmp = nums[i];
      nums[i] = nums[v - 1];
      nums[v - 1] = tmp;
    } else {
      i++;
    }
  }
  for (var j = 0; j < n; j++) {
    if (nums[j] != j + 1) return j + 1;
  }
  return n + 1;
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty || lines[0].trim().isEmpty) return;
  var idx = 0;
  final t = int.parse(lines[idx++].trim());
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(lines[idx++].trim());
    final nums = <int>[];
    for (var i = 0; i < n; i++) nums.add(int.parse(lines[idx++].trim()));
    out.add(firstMissingPositive(nums).toString());
  }
  stdout.write(out.join('\n'));
}
