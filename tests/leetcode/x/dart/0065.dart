import 'dart:convert';
import 'dart:io';

bool isNumber(String s) {
  var seenDigit = false, seenDot = false, seenExp = false, digitAfterExp = true;
  for (var i = 0; i < s.length; i++) {
    final ch = s[i];
    if (ch.compareTo('0') >= 0 && ch.compareTo('9') <= 0) {
      seenDigit = true;
      if (seenExp) digitAfterExp = true;
    } else if (ch == '+' || ch == '-') {
      if (i != 0 && s[i - 1] != 'e' && s[i - 1] != 'E') return false;
    } else if (ch == '.') {
      if (seenDot || seenExp) return false;
      seenDot = true;
    } else if (ch == 'e' || ch == 'E') {
      if (seenExp || !seenDigit) return false;
      seenExp = true;
      digitAfterExp = false;
    } else return false;
  }
  return seenDigit && digitAfterExp;
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isNotEmpty) {
    final t = int.parse(lines[0].trim());
    final out = <String>[];
    for (var i = 0; i < t; i++) out.add(isNumber(lines[i + 1]) ? 'true' : 'false');
    stdout.write(out.join('\n'));
  }
}
