import 'dart:io';

String solve(List<String> words) {
  final chars = <String>{};
  for (final w in words) {
    for (final c in w.split('')) chars.add(c);
  }
  final adj = <String, Set<String>>{};
  final indeg = <String, int>{};
  for (final c in chars) {
    adj[c] = <String>{};
    indeg[c] = 0;
  }
  for (var i = 0; i + 1 < words.length; i++) {
    final a = words[i], b = words[i + 1];
    final m = a.length < b.length ? a.length : b.length;
    if (a.substring(0, m) == b.substring(0, m) && a.length > b.length) return '';
    for (var j = 0; j < m; j++) {
      if (a[j] != b[j]) {
        if (!adj[a[j]]!.contains(b[j])) {
          adj[a[j]]!.add(b[j]);
          indeg[b[j]] = indeg[b[j]]! + 1;
        }
        break;
      }
    }
  }
  final zeros = chars.where((c) => indeg[c] == 0).toList()..sort();
  final out = <String>[];
  while (zeros.isNotEmpty) {
    final c = zeros.removeAt(0);
    out.add(c);
    final nexts = adj[c]!.toList()..sort();
    for (final nei in nexts) {
      indeg[nei] = indeg[nei]! - 1;
      if (indeg[nei] == 0) {
        zeros.add(nei);
        zeros.sort();
      }
    }
  }
  return out.length == chars.length ? out.join() : '';
}

void main() {
  final lines = File('/dev/stdin').readAsLinesSync();
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  final out = <String>[];
  var idx = 1;
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(lines[idx++].trim());
    out.add(solve(lines.sublist(idx, idx + n).map((s) => s.trim()).toList()));
    idx += n;
  }
  stdout.write(out.join('\n'));
}
