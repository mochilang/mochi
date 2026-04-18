import 'dart:io';

int solve(List<int> a) {
  var best = 0;
  for (var i = 0; i < a.length; i++) {
    var mn = a[i];
    for (var j = i; j < a.length; j++) {
      if (a[j] < mn) mn = a[j];
      final area = mn * (j - i + 1);
      if (area > best) best = area;
    }
  }
  return best;
}

void main() {
  final data = <String>[];
  String? line;
  while ((line = stdin.readLineSync()) != null) {
    for (final part in line!.trim().split(RegExp(r'\s+'))) {
      if (part.isNotEmpty) data.add(part);
    }
  }
  if (data.isEmpty) return;
  var idx = 0;
  final t = int.parse(data[idx++]);
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(data[idx++]);
    final a = <int>[];
    for (var i = 0; i < n; i++) a.add(int.parse(data[idx++]));
    out.add('${solve(a)}');
  }
  stdout.write(out.join('\n'));
}
