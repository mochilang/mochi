import 'dart:convert';
import 'dart:io';

List<int> twoSum(List<int> nums, int target) {
  for (int i = 0; i < nums.length; i++) {
    for (int j = i + 1; j < nums.length; j++) {
      if (nums[i] + nums[j] == target) {
        return [i, j];
      }
    }
  }
  return [0, 0];
}

Future<void> main() async {
  final data = (await stdin.transform(utf8.decoder).join()).trim();
  if (data.isEmpty) {
    return;
  }
  final tokens = data.split(RegExp(r'\s+'));
  int idx = 0;
  final t = int.parse(tokens[idx++]);
  final lines = <String>[];
  for (int tc = 0; tc < t; tc++) {
    final n = int.parse(tokens[idx++]);
    final target = int.parse(tokens[idx++]);
    final nums = <int>[];
    for (int i = 0; i < n; i++) {
      nums.add(int.parse(tokens[idx++]));
    }
    final ans = twoSum(nums, target);
    lines.add('${ans[0]} ${ans[1]}');
  }
  stdout.write(lines.join('\n'));
}
