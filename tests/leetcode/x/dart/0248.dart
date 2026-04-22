import 'dart:io';

const pairs = [
  ['0', '0'],
  ['1', '1'],
  ['6', '9'],
  ['8', '8'],
  ['9', '6'],
];

List<String> build(int n, int m) {
  if (n == 0) return [''];
  if (n == 1) return ['0', '1', '8'];
  final mids = build(n - 2, m);
  final res = <String>[];
  for (final mid in mids) {
    for (final p in pairs) {
      if (n == m && p[0] == '0') continue;
      res.add('${p[0]}$mid${p[1]}');
    }
  }
  return res;
}

int countRange(String low, String high) {
  var ans = 0;
  for (var len = low.length; len <= high.length; len++) {
    for (final s in build(len, len)) {
      if (len == low.length && s.compareTo(low) < 0) continue;
      if (len == high.length && s.compareTo(high) > 0) continue;
      ans++;
    }
  }
  return ans;
}

void main() {
  final lines = File('/dev/stdin').readAsLinesSync();
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  final out = <String>[];
  var idx = 1;
  for (var i = 0; i < t; i++) {
    out.add(countRange(lines[idx].trim(), lines[idx + 1].trim()).toString());
    idx += 2;
  }
  stdout.write(out.join('\n'));
}
