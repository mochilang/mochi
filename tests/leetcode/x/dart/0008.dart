import 'dart:convert';
import 'dart:io';

int myAtoi(String s) {
  var i = 0;
  while (i < s.length && s[i] == ' ') i++;
  var sign = 1;
  if (i < s.length && (s[i] == '+' || s[i] == '-')) {
    if (s[i] == '-') sign = -1;
    i++;
  }
  var ans = 0;
  final limit = sign > 0 ? 7 : 8;
  while (i < s.length) {
    final code = s.codeUnitAt(i);
    if (code < 48 || code > 57) break;
    final digit = code - 48;
    if (ans > 214748364 || (ans == 214748364 && digit > limit)) {
      return sign > 0 ? 2147483647 : -2147483648;
    }
    ans = ans * 10 + digit;
    i++;
  }
  return sign * ans;
}

Future<void> main() async {
  final data = await stdin.transform(utf8.decoder).join();
  final lines = const LineSplitter().convert(data);
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  final out = <String>[];
  for (var i = 0; i < t; i++) out.add(myAtoi(i + 1 < lines.length ? lines[i + 1] : '').toString());
  stdout.write(out.join('\n'));
}
