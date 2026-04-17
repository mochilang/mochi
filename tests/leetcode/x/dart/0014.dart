import 'dart:convert';
import 'dart:io';

String lcp(List<String> strs) {
  var prefix = strs[0];
  while (!strs.every((s) => s.startsWith(prefix))) {
    prefix = prefix.substring(0, prefix.length - 1);
  }
  return prefix;
}

Future<void> main() async {
  final data = (await stdin.transform(utf8.decoder).join()).trim();
  if (data.isEmpty) return;
  final tokens = data.split(RegExp(r'\s+'));
  int idx = 0;
  final t = int.parse(tokens[idx++]);
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(tokens[idx++]);
    final strs = tokens.sublist(idx, idx + n);
    idx += n;
    out.add('"${lcp(strs)}"');
  }
  stdout.write(out.join('\n'));
}
