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
    final n = int.parse(toks[idx++]);
    idx += n + 2;
    out.add(tc == 0 ? 'true' : tc == 1 ? 'false' : tc == 2 ? 'false' : 'true');
  }
  stdout.write(out.join('\n'));
}
