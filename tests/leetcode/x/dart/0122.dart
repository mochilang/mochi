import 'dart:convert';
import 'dart:io';

int maxProfit(List<int> prices) {
  var best = 0;
  for (var i = 1; i < prices.length; i++) {
    if (prices[i] > prices[i - 1]) best += prices[i] - prices[i - 1];
  }
  return best;
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
