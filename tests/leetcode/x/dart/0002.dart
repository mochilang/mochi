import 'dart:convert';
import 'dart:io';

List<int> addLists(List<int> a, List<int> b) {
  final out = <int>[];
  var i = 0, j = 0, carry = 0;
  while (i < a.length || j < b.length || carry > 0) {
    var sum = carry;
    if (i < a.length) sum += a[i++];
    if (j < b.length) sum += b[j++];
    out.add(sum % 10);
    carry = sum ~/ 10;
  }
  return out;
}

String fmt(List<int> a) => '[${a.join(',')}]';

Future<void> main() async {
  final data = (await stdin.transform(utf8.decoder).join()).trim();
  if (data.isEmpty) return;
  final tokens = data.split(RegExp(r'\s+'));
  var idx = 0;
  final t = int.parse(tokens[idx++]);
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(tokens[idx++]);
    final a = <int>[];
    for (var i = 0; i < n; i++) a.add(int.parse(tokens[idx++]));
    final m = int.parse(tokens[idx++]);
    final b = <int>[];
    for (var i = 0; i < m; i++) b.add(int.parse(tokens[idx++]));
    out.add(fmt(addLists(a, b)));
  }
  stdout.write(out.join('\n'));
}
