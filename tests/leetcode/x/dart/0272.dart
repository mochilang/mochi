import 'dart:io';
import 'dart:math';

List<int> solve(List<int> values, double target, int k) {
  var right = 0;
  while (right < values.length && values[right] < target) right++;
  var left = right - 1;
  final ans = <int>[];
  while (ans.length < k) {
    if (left < 0) {
      ans.add(values[right++]);
    } else if (right >= values.length) {
      ans.add(values[left--]);
    } else if ((values[left] - target).abs() <= (values[right] - target).abs()) {
      ans.add(values[left--]);
    } else {
      ans.add(values[right++]);
    }
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
    final values = <int>[];
    for (var i = 0; i < n; i++) values.add(int.parse(toks[idx++]));
    final target = double.parse(toks[idx++]);
    final k = int.parse(toks[idx++]);
    final ans = solve(values, target, k);
    blocks.add(([ans.length.toString()] + ans.map((x) => x.toString()).toList()).join('\n'));
  }
  stdout.write(blocks.join('\n\n'));
}
