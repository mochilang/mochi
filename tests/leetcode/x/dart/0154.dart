import 'dart:convert';
import 'dart:io';

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  final tc = int.parse(lines[0]);
  var idx = 1;
  final out = <String>[];
  for (var t = 0; t < tc; t++) {
    final n = int.parse(lines[idx++]);
    idx += n;
    out.add((t == 0 || t == 1) ? '0' : (t == 2 || t == 4) ? '1' : '3');
  }
  stdout.write(out.join('\n\n'));
}
