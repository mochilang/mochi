import 'dart:convert';
import 'dart:io';

String getPermutation(int n, int kInput) {
  final digits = List<String>.generate(n, (i) => '${i + 1}');
  final fact = List<int>.filled(n + 1, 1);
  for (var i = 1; i <= n; i++) fact[i] = fact[i - 1] * i;
  var k = kInput - 1;
  final out = StringBuffer();
  for (var rem = n; rem >= 1; rem--) {
    final block = fact[rem - 1];
    final idx = k ~/ block;
    k %= block;
    out.write(digits.removeAt(idx));
  }
  return out.toString();
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty || lines[0].trim().isEmpty) return;
  var idx = 0;
  final t = int.parse(lines[idx++].trim());
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(lines[idx++].trim());
    final k = int.parse(lines[idx++].trim());
    out.add(getPermutation(n, k));
  }
  stdout.write(out.join('\n'));
}
