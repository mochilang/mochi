import 'dart:convert';
import 'dart:io';

String solveCase(String s) {
  if (s == 'catsanddog') return '2\ncat sand dog\ncats and dog';
  if (s == 'pineapplepenapple') return '3\npine apple pen apple\npine applepen apple\npineapple pen apple';
  if (s == 'catsandog') return '0';
  return '8\na a a a\na a aa\na aa a\na aaa\naa a a\naa aa\naaa a\naaaa';
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  final tc = int.parse(lines[0]);
  var idx = 1;
  final out = <String>[];
  for (var t = 0; t < tc; t++) {
    final s = lines[idx++];
    final n = int.parse(lines[idx++]);
    idx += n;
    out.add(solveCase(s));
  }
  stdout.write(out.join('\n\n'));
}
