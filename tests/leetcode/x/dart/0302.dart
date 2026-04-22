import 'dart:io';

void main() async {
  final lines = (await stdin.transform(SystemEncoding().decoder).join())
      .split(RegExp(r'\r?\n'))
      .map((e) => e.trim())
      .where((e) => e.isNotEmpty)
      .toList();
  if (lines.isEmpty) return;
  final t = int.parse(lines[0]);
  var idx = 1;
  final blocks = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final parts = lines[idx++].split(RegExp(r'\s+'));
    final r = int.parse(parts[0]);
    final image = lines.sublist(idx, idx + r);
    idx += r;
    idx++; // skip x y
    var top = r, bottom = -1, left = image[0].length, right = -1;
    for (var i = 0; i < image.length; i++) {
      for (var j = 0; j < image[i].length; j++) {
        if (image[i][j] == '1') {
          if (i < top) top = i;
          if (i > bottom) bottom = i;
          if (j < left) left = j;
          if (j > right) right = j;
        }
      }
    }
    blocks.add(((bottom - top + 1) * (right - left + 1)).toString());
  }
  stdout.write(blocks.join('\n\n'));
}
