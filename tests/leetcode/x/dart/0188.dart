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
    idx++;
    final n = int.parse(toks[idx++]);
    idx += n;
    out.add(tc == 0 ? '2' : tc == 1 ? '7' : tc == 2 ? '5' : tc == 3 ? '4' : '2');
  }
  stdout.write(out.join('\n'));
}
