import 'dart:convert';
import 'dart:io';

const int intMin = -2147483648;
const int intMax = 2147483647;

int reverseInt(int x) {
  var ans = 0;
  while (x != 0) {
    final digit = x.remainder(10);
    x ~/= 10;
    if (ans > intMax ~/ 10 || (ans == intMax ~/ 10 && digit > 7)) return 0;
    if (ans < intMin ~/ 10 || (ans == intMin ~/ 10 && digit < -8)) return 0;
    ans = ans * 10 + digit;
  }
  return ans;
}

Future<void> main() async {
  final data = await stdin.transform(utf8.decoder).join();
  final lines = const LineSplitter().convert(data);
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  final out = <String>[];
  for (var i = 0; i < t; i++) {
    final x = i + 1 < lines.length ? int.parse(lines[i + 1].trim()) : 0;
    out.add(reverseInt(x).toString());
  }
  stdout.write(out.join('\n'));
}
