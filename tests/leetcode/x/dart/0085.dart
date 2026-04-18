import 'dart:io';

int hist(List<int> h) {
  var best = 0;
  for (var i = 0; i < h.length; i++) {
    var mn = h[i];
    for (var j = i; j < h.length; j++) {
      if (h[j] < mn) mn = h[j];
      final area = mn * (j - i + 1);
      if (area > best) best = area;
    }
  }
  return best;
}

void main() {
  final toks = <String>[];
  String? line;
  while ((line = stdin.readLineSync()) != null) {
    for (final p in line!.trim().split(RegExp(r'\s+'))) {
      if (p.isNotEmpty) toks.add(p);
    }
  }
  if (toks.isEmpty) return;
  var idx = 0;
  final t = int.parse(toks[idx++]);
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final rows = int.parse(toks[idx++]), cols = int.parse(toks[idx++]);
    final h = List.filled(cols, 0);
    var best = 0;
    for (var r = 0; r < rows; r++) {
      final s = toks[idx++];
      for (var c = 0; c < cols; c++) h[c] = s[c] == '1' ? h[c] + 1 : 0;
      final area = hist(h);
      if (area > best) best = area;
    }
    out.add('$best');
  }
  stdout.write(out.join('\n'));
}
