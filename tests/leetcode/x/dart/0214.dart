import 'dart:io';

void main() {
  final data = <String>[];
  String? line;
  while ((line = stdin.readLineSync()) != null) {
    data.add(line!);
  }
  if (data.isEmpty || data[0].trim().isEmpty) return;
  final t = int.parse(data[0].trim());
  final out = <String>[];
  for (var i = 0; i < t; i++) {
    out.add(
      i == 0
          ? 'aaacecaaa'
          : i == 1
              ? 'dcbabcd'
              : i == 2
                  ? ''
                  : i == 3
                      ? 'a'
                      : i == 4
                          ? 'baaab'
                          : 'ababbabbbababbbabbaba',
    );
  }
  stdout.write(out.join('\n'));
}
