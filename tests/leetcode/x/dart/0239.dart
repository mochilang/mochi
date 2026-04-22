import 'dart:io';

List<int> solve(List<int> nums, int k) {
  final dq = <int>[];
  final ans = <int>[];
  for (var i = 0; i < nums.length; i++) {
    while (dq.isNotEmpty && dq.first <= i - k) {
      dq.removeAt(0);
    }
    while (dq.isNotEmpty && nums[dq.last] <= nums[i]) {
      dq.removeLast();
    }
    dq.add(i);
    if (i >= k - 1) ans.add(nums[dq.first]);
  }
  return ans;
}

void main() {
  final toks = File('/dev/stdin').readAsStringSync().trim().split(RegExp(r'\s+')).where((s) => s.isNotEmpty).toList();
  if (toks.isEmpty) return;
  var idx = 0;
  final t = int.parse(toks[idx++]);
  final blocks = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(toks[idx++]);
    final nums = <int>[];
    for (var i = 0; i < n; i++) nums.add(int.parse(toks[idx++]));
    final k = int.parse(toks[idx++]);
    final ans = solve(nums, k);
    blocks.add(([ans.length.toString()] + ans.map((x) => x.toString()).toList()).join('\n'));
  }
  stdout.write(blocks.join('\n\n'));
}
