import 'dart:convert';
import 'dart:io';

List<List<String>> ladders(String begin, String end, List<String> words) {
  final wordSet = words.toSet();
  if (!wordSet.contains(end)) return [];
  final parents = <String, List<String>>{};
  var level = <String>{begin};
  final visited = <String>{begin};
  var found = false;
  while (level.isNotEmpty && !found) {
    final next = <String>{};
    for (final word in level.toList()..sort()) {
      final chars = word.split('');
      for (var i = 0; i < chars.length; i++) {
        final orig = chars[i];
        for (var c = 97; c <= 122; c++) {
          final ch = String.fromCharCode(c);
          if (ch == orig) continue;
          chars[i] = ch;
          final nw = chars.join();
          if (!wordSet.contains(nw) || visited.contains(nw)) continue;
          next.add(nw);
          parents.putIfAbsent(nw, () => []).add(word);
          if (nw == end) found = true;
        }
        chars[i] = orig;
      }
    }
    visited.addAll(next);
    level = next;
  }
  if (!found) return [];
  final out = <List<String>>[];
  final path = <String>[end];
  void backtrack(String word) {
    if (word == begin) {
      out.add(path.reversed.toList());
      return;
    }
    final plist = [...?parents[word]]..sort();
    for (final p in plist) {
      path.add(p);
      backtrack(p);
      path.removeLast();
    }
  }
  backtrack(end);
  out.sort((a, b) => a.join('->').compareTo(b.join('->')));
  return out;
}
String fmt(List<List<String>> paths) {
  final lines = <String>[paths.length.toString()];
  for (final p in paths) lines.add(p.join('->'));
  return lines.join('\n');
}
Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  var idx = 1;
  final tc = int.parse(lines[0]);
  final out = <String>[];
  for (var t = 0; t < tc; t++) {
    final begin = lines[idx++], end = lines[idx++];
    final n = int.parse(lines[idx++]);
    final words = lines.sublist(idx, idx + n); idx += n;
    out.add(fmt(ladders(begin, end, words)));
  }
  stdout.write(out.join('\n\n'));
}
