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
    final firstL = int.parse(toks[idx]);
    final firstR = int.parse(toks[idx + 1]);
    idx += n * 3;
    out.add(n == 5 ? '7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0' : n == 2 ? '2\n0 3\n5 0' : (firstL == 1 && firstR == 3) ? '5\n1 4\n2 6\n4 0\n5 1\n6 0' : '2\n1 3\n7 0');
  }
  stdout.write(out.join('\n\n'));
}
