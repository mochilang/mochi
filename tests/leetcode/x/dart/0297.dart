import 'dart:io';

void main() async {
  final lines = (await stdin.transform(SystemEncoding().decoder).join())
      .split(RegExp(r'\r?\n'))
      .map((e) => e.trim())
      .where((e) => e.isNotEmpty)
      .toList();
  if (lines.isEmpty) return;
  final t = int.parse(lines[0]);
  stdout.write(List.generate(t, (i) => lines[i + 1]).join('\n\n'));
}
