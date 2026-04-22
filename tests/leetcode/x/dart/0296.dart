import 'dart:io';

void main() {
  final data = stdin.transform(SystemEncoding().decoder).join().then((s) {
    final nums = s.trim().split(RegExp(r'\s+')).where((e) => e.isNotEmpty).map(int.parse).toList();
    if (nums.isEmpty) return;
    var idx = 0;
    final t = nums[idx++];
    final blocks = <String>[];
    for (var tc = 0; tc < t; tc++) {
      final r = nums[idx++];
      final c = nums[idx++];
      final grid = List.generate(r, (_) => List.filled(c, 0));
      final rows = <int>[];
      final cols = <int>[];
      for (var i = 0; i < r; i++) {
        for (var j = 0; j < c; j++) {
          grid[i][j] = nums[idx++];
          if (grid[i][j] == 1) rows.add(i);
        }
      }
      for (var j = 0; j < c; j++) {
        for (var i = 0; i < r; i++) {
          if (grid[i][j] == 1) cols.add(j);
        }
      }
      final mr = rows[rows.length ~/ 2];
      final mc = cols[cols.length ~/ 2];
      final ans = rows.fold(0, (s, x) => s + (x - mr).abs()) + cols.fold(0, (s, x) => s + (x - mc).abs());
      blocks.add(ans.toString());
    }
    stdout.write(blocks.join('\n\n'));
  });
}
