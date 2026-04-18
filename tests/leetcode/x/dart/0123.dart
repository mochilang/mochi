import 'dart:convert';
import 'dart:io';

int maxProfit(List<int> prices) {
  var buy1 = -1 << 60, sell1 = 0, buy2 = -1 << 60, sell2 = 0;
  for (final p in prices) {
    if (-p > buy1) buy1 = -p;
    if (buy1 + p > sell1) sell1 = buy1 + p;
    if (sell1 - p > buy2) buy2 = sell1 - p;
    if (buy2 + p > sell2) sell2 = buy2 + p;
  }
  return sell2;
}

Future<void> main() async {
  final lines = const LineSplitter().convert(await stdin.transform(utf8.decoder).join());
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  var idx = 1;
  final out = <String>[];
  for (var tc = 0; tc < t; tc++) {
    final n = int.parse(lines[idx++].trim());
    final prices = <int>[];
    for (var i = 0; i < n; i++) {
      prices.add(int.parse(lines[idx++].trim()));
    }
    out.add(maxProfit(prices).toString());
  }
  stdout.write(out.join('\n'));
}
