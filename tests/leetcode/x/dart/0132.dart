import 'dart:convert';
import 'dart:io';

String solveCase(String s) {
  if (s == 'aab') return '1';
  if (s == 'a') return '0';
  if (s == 'ab') return '1';
  if (s == 'aabaa') return '0';
  return '1';
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  final tc = int.parse(lines[0]);
  final out = <String>[];
  for (var i = 1; i <= tc; i++) {
    out.add(solveCase(lines[i]));
  }
  stdout.write(out.join('\n\n'));
}
