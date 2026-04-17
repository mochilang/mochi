import 'dart:convert';
import 'dart:io';

const values = {'I': 1, 'V': 5, 'X': 10, 'L': 50, 'C': 100, 'D': 500, 'M': 1000};

int romanToInt(String s) {
  var total = 0;
  for (var i = 0; i < s.length; i++) {
    final cur = values[s[i]]!;
    final next = i + 1 < s.length ? values[s[i + 1]]! : 0;
    total += cur < next ? -cur : cur;
  }
  return total;
}

Future<void> main() async {
  final data = (await stdin.transform(utf8.decoder).join()).trim();
  if (data.isEmpty) return;
  final tokens = data.split(RegExp(r'\s+'));
  final t = int.parse(tokens[0]);
  final out = <String>[];
  for (var i = 0; i < t; i++) {
    out.add('${romanToInt(tokens[i + 1])}');
  }
  stdout.write(out.join('\n'));
}
