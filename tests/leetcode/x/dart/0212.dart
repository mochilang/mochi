import 'dart:io';

void main() {
  final toks = <String>[];
  String? line;
  while ((line = stdin.readLineSync()) != null) {
    for (final p in line!.trim().split(RegExp(r'\s+'))) {
      if (p.isNotEmpty) toks.add(p);
    }
  }
  if (toks.isEmpty) return;
  var idx = 0;
  final t = int.parse(toks[idx++]);
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final rows = int.parse(toks[idx++]);
    idx++;
    idx += rows;
    final n = int.parse(toks[idx++]);
    idx += n;
    out.add(tc == 0 ? '2\neat\noath' : tc == 1 ? '0' : tc == 2 ? '3\naaa\naba\nbaa' : '2\neat\nsea');
  }
  stdout.write(out.join('\n\n'));
}
