import 'dart:convert';
import 'dart:io';

int maxProfit(List<int> prices) {
  if (prices.isEmpty) return 0;
  var minPrice = prices[0], best = 0;
  for (var i = 1; i < prices.length; i++) {
    final profit = prices[i] - minPrice;
    if (profit > best) best = profit;
    if (prices[i] < minPrice) minPrice = prices[i];
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
