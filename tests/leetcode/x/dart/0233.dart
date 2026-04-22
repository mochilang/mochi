import 'dart:io';

int countDigitOne(int n) {
  var total = 0;
  var m = 1;
  while (m <= n) {
    final high = n ~/ (m * 10);
    final cur = (n ~/ m) % 10;
    final low = n % m;
    if (cur == 0) {
      total += high * m;
    } else if (cur == 1) {
      total += high * m + low + 1;
    } else {
      total += (high + 1) * m;
    }
    m *= 10;
  }
  return total;
}

void main() {
  final lines = File('/dev/stdin').readAsLinesSync();
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  final out = <String>[];
  for (var i = 0; i < t; i++) {
    out.add(countDigitOne(int.parse(lines[i + 1].trim())).toString());
  }
  stdout.write(out.join('\n'));
}
