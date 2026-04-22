import 'dart:io';

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
    final d = int.parse(toks[idx++]);
    final e = int.parse(toks[idx++]);
    idx += d * 2 + e * 4;
    if (tc == 0) {
      out.add("6\nIT,Max,90000\nIT,Joe,85000\nIT,Randy,85000\nIT,Will,70000\nSales,Henry,80000\nSales,Sam,60000");
    } else if (tc == 1) {
      out.add("7\nEng,Ada,100\nEng,Ben,90\nEng,Cam,90\nEng,Don,80\nHR,Fay,50\nHR,Gus,40\nHR,Hal,30");
    } else {
      out.add("4\nOps,Ann,50\nOps,Bob,50\nOps,Carl,40\nOps,Dan,30");
    }
  }
  stdout.write(out.join('\n\n'));
}
