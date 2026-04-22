import 'dart:io';

class MedianFinder {
  final List<int> data = [];

  void addNum(int num) {
    int lo = 0, hi = data.length;
    while (lo < hi) {
      final mid = (lo + hi) >> 1;
      if (data[mid] < num) {
        lo = mid + 1;
      } else {
        hi = mid;
      }
    }
    data.insert(lo, num);
  }

  double findMedian() {
    final n = data.length;
    if (n.isOdd) return data[n ~/ 2].toDouble();
    return (data[n ~/ 2 - 1] + data[n ~/ 2]) / 2.0;
  }
}

void main() {
  final lines = stdin
      .transform(SystemEncoding().decoder)
      .join()
      .then((s) => s.split(RegExp(r'\r?\n')).map((e) => e.trim()).where((e) => e.isNotEmpty).toList())
      .then((lines) {
    if (lines.isEmpty) return;
    final t = int.parse(lines[0]);
    int idx = 1;
    final blocks = <String>[];
    for (var tc = 0; tc < t; tc++) {
      final m = int.parse(lines[idx++]);
      final mf = MedianFinder();
      final out = <String>[];
      for (var i = 0; i < m; i++) {
        final parts = lines[idx++].split(RegExp(r'\s+'));
        if (parts[0] == 'addNum') {
          mf.addNum(int.parse(parts[1]));
        } else {
          out.add(mf.findMedian().toStringAsFixed(1));
        }
      }
      blocks.add(out.join('\n'));
    }
    stdout.write(blocks.join('\n\n'));
  });
}
