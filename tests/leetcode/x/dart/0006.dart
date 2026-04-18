import 'dart:convert';
import 'dart:io';

String convertZigzag(String s, int numRows) {
  if (numRows <= 1 || numRows >= s.length) return s;
  final cycle = 2 * numRows - 2;
  final out = StringBuffer();
  for (var row = 0; row < numRows; row++) {
    for (var i = row; i < s.length; i += cycle) {
      out.write(s[i]);
      final diag = i + cycle - 2 * row;
      if (row > 0 && row < numRows - 1 && diag < s.length) out.write(s[diag]);
    }
  }
  return out.toString();
}

Future<void> main() async {
  final data = await stdin.transform(utf8.decoder).join();
  final lines = const LineSplitter().convert(data);
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  final out = <String>[];
  var idx = 1;
  for (var i = 0; i < t; i++) {
    final s = idx < lines.length ? lines[idx] : '';
    idx += 1;
    final numRows = idx < lines.length ? int.parse(lines[idx].trim()) : 1;
    idx += 1;
    out.add(convertZigzag(s, numRows));
  }
  stdout.write(out.join('\n'));
}
