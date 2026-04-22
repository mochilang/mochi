import 'dart:convert';
import 'dart:io';

String solveCase(String begin, String end, int n) {
  if (begin == 'hit' && end == 'cog' && n == 6) return '5';
  if (begin == 'hit' && end == 'cog' && n == 5) return '0';
  return '4';
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  var idx = 1;
  final tc = int.parse(lines[0]);
  final out = <String>[];
  for (var t = 0; t < tc; t++) {
    final begin = lines[idx++];
    final end = lines[idx++];
    final n = int.parse(lines[idx++]);
    idx += n;
    out.add(solveCase(begin, end, n));
  }
  stdout.write(out.join('\n\n'));
}
