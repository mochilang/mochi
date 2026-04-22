import 'dart:convert';
import 'dart:io';

int calculate(String expr) {
  var result = 0;
  var number = 0;
  var sign = 1;
  final stack = <int>[];
  for (final ch in expr.split('')) {
    if (ch.codeUnitAt(0) >= 48 && ch.codeUnitAt(0) <= 57) {
      number = number * 10 + (ch.codeUnitAt(0) - 48);
    } else if (ch == '+' || ch == '-') {
      result += sign * number;
      number = 0;
      sign = ch == '+' ? 1 : -1;
    } else if (ch == '(') {
      stack..add(result)..add(sign);
      result = 0;
      number = 0;
      sign = 1;
    } else if (ch == ')') {
      result += sign * number;
      number = 0;
      final prevSign = stack.removeLast();
      final prevResult = stack.removeLast();
      result = prevResult + prevSign * result;
    }
  }
  return result + sign * number;
}

void main() {
  final lines = const LineSplitter().convert(
    utf8.decode(File('/dev/stdin').readAsBytesSync()),
  );
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  final out = <String>[];
  for (var i = 0; i < t; i++) {
    out.add(calculate(lines[i + 1]).toString());
  }
  stdout.write(out.join('\n'));
}
