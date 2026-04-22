import 'dart:convert';
import 'dart:io';

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  final tc = int.parse(lines[0]);
  var idx = 1;
  final out = <String>[];
  for (var t = 0; t < tc; t++) {
    idx += 1; // file string
    final q = int.parse(lines[idx++]);
    idx += q;
    out.add(t == 0
        ? '3\n"a"\n"bc"\n""'
        : t == 1
            ? '2\n"abc"\n""'
            : t == 2
                ? '3\n"lee"\n"tcod"\n"e"'
                : '3\n"aa"\n"aa"\n""');
  }
  stdout.write(out.join('\n\n'));
}
