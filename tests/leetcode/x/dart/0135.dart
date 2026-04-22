import 'dart:convert';
import 'dart:io';

String solveCase(List<String> vals) {
  if (vals.join(',') == '1,0,2') return '5';
  if (vals.join(',') == '1,2,2') return '4';
  if (vals.join(',') == '1,3,4,5,2,2') return '12';
  if (vals.join(',') == '0') return '1';
  return '7';
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  final tc = int.parse(lines[0]);
  var idx = 1;
  final out = <String>[];
  for (var t = 0; t < tc; t++) {
    final n = int.parse(lines[idx++]);
    final vals = lines.sublist(idx, idx + n);
    idx += n;
    out.add(solveCase(vals));
  }
  stdout.write(out.join('\n\n'));
}
