import 'dart:convert';
import 'dart:io';

bool isPalindrome(int x) {
  if (x < 0) return false;
  final original = x;
  var n = x;
  var rev = 0;
  while (n > 0) {
    rev = rev * 10 + (n % 10);
    n ~/= 10;
  }
  return rev == original;
}

Future<void> main() async {
  final data = (await stdin.transform(utf8.decoder).join()).trim();
  if (data.isEmpty) return;
  final tokens = data.split(RegExp(r'\s+'));
  int idx = 0;
  final t = int.parse(tokens[idx++]);
  final out = <String>[];
  for (int i = 0; i < t; i++) {
    out.add(isPalindrome(int.parse(tokens[idx++])) ? 'true' : 'false');
  }
  stdout.write(out.join('\n'));
}
