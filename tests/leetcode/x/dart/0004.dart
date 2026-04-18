import 'dart:convert';
import 'dart:io';

double median(List<int> a, List<int> b) {
  final m = <int>[];
  var i = 0, j = 0;
  while (i < a.length && j < b.length) {
    if (a[i] <= b[j]) { m.add(a[i]); i++; } else { m.add(b[j]); j++; }
  }
  while (i < a.length) { m.add(a[i]); i++; }
  while (j < b.length) { m.add(b[j]); j++; }
  if (m.length.isOdd) return m[m.length ~/ 2].toDouble();
  return (m[m.length ~/ 2 - 1] + m[m.length ~/ 2]) / 2.0;
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  var idx = 1;
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(lines[idx++].trim());
    final a = <int>[]; for (var i = 0; i < n; i++) a.add(int.parse(lines[idx++].trim()));
    final m = int.parse(lines[idx++].trim());
    final b = <int>[]; for (var i = 0; i < m; i++) b.add(int.parse(lines[idx++].trim()));
    out.add(median(a,b).toStringAsFixed(1));
  }
  stdout.write(out.join('\n'));
}
