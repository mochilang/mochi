import 'dart:io';

const less20 = [
  '', 'One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine',
  'Ten', 'Eleven', 'Twelve', 'Thirteen', 'Fourteen', 'Fifteen', 'Sixteen', 'Seventeen', 'Eighteen', 'Nineteen'
];
const tens = ['', '', 'Twenty', 'Thirty', 'Forty', 'Fifty', 'Sixty', 'Seventy', 'Eighty', 'Ninety'];
const thousands = ['', 'Thousand', 'Million', 'Billion'];

String helper(int n) {
  if (n == 0) return '';
  if (n < 20) return less20[n];
  if (n < 100) return tens[n ~/ 10] + (n % 10 == 0 ? '' : ' ${helper(n % 10)}');
  return less20[n ~/ 100] + ' Hundred' + (n % 100 == 0 ? '' : ' ${helper(n % 100)}');
}

String solve(int num) {
  if (num == 0) return 'Zero';
  final parts = <String>[];
  var idx = 0;
  while (num > 0) {
    final chunk = num % 1000;
    if (chunk != 0) {
      var words = helper(chunk);
      if (thousands[idx].isNotEmpty) words += ' ${thousands[idx]}';
      parts.insert(0, words);
    }
    num ~/= 1000;
    idx++;
  }
  return parts.join(' ');
}

void main() {
  final lines = File('/dev/stdin').readAsLinesSync();
  if (lines.isEmpty) return;
  final t = int.parse(lines[0].trim());
  final out = <String>[];
  for (var i = 0; i < t; i++) out.add(solve(int.parse(lines[i + 1].trim())));
  stdout.write(out.join('\n'));
}
