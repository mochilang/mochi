import 'dart:convert';
import 'dart:io';

int longest(String s) {
  final last = <String, int>{};
  var left = 0;
  var best = 0;
  for (var right = 0; right < s.length; right++) {
    final ch = s[right];
    if (last.containsKey(ch) && last[ch]! >= left) left = last[ch]! + 1;
    last[ch] = right;
    if (right - left + 1 > best) best = right - left + 1;
  }
  return best;
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  final out = <String>[];
  for (var i = 0; i < t; i++) out.add('${longest(i + 1 < lines.length ? lines[i + 1] : '')}');
  stdout.write(out.join('\n'));
}
