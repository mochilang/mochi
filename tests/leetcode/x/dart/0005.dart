import 'dart:convert';
import 'dart:io';

List<int> expand(String s, int left, int right) {
  while (left >= 0 && right < s.length && s[left] == s[right]) {
    left--;
    right++;
  }
  return [left + 1, right - left - 1];
}

String longestPalindrome(String s) {
  var bestStart = 0;
  var bestLen = s.isEmpty ? 0 : 1;
  for (var i = 0; i < s.length; i++) {
    var odd = expand(s, i, i);
    if (odd[1] > bestLen) {
      bestStart = odd[0];
      bestLen = odd[1];
    }
    var even = expand(s, i, i + 1);
    if (even[1] > bestLen) {
      bestStart = even[0];
      bestLen = even[1];
    }
  }
  return s.substring(bestStart, bestStart + bestLen);
}

Future<void> main() async {
  var data = await stdin.transform(utf8.decoder).join();
  var lines = const LineSplitter().convert(data);
  if (lines.isEmpty) return;
  var t = int.parse(lines[0].trim());
  var out = <String>[];
  for (var i = 0; i < t; i++) {
    var s = i + 1 < lines.length ? lines[i + 1] : '';
    out.add(longestPalindrome(s));
  }
  stdout.write(out.join('\n'));
}
