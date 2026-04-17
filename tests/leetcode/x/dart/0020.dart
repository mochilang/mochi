import 'dart:convert';
import 'dart:io';

bool isValid(String s) {
  final stack = <String>[];
  for (final ch in s.split('')) {
    if (ch == '(' || ch == '[' || ch == '{') {
      stack.add(ch);
    } else {
      if (stack.isEmpty) return false;
      final open = stack.removeLast();
      if ((ch == ')' && open != '(') ||
          (ch == ']' && open != '[') ||
          (ch == '}' && open != '{')) return false;
    }
  }
  return stack.isEmpty;
}

Future<void> main() async {
  final data = (await stdin.transform(utf8.decoder).join()).trim();
  if (data.isEmpty) return;
  final tokens = data.split(RegExp(r'\s+'));
  final t = int.parse(tokens[0]);
  final out = <String>[];
  for (var i = 0; i < t; i++) {
    out.add(isValid(tokens[i + 1]) ? 'true' : 'false');
  }
  stdout.write(out.join('\n'));
}
