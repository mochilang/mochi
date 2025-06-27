import 'dart:io';

bool isMatch(String s, String p) {
	int m = s.length;
	int n = p.length;
	List<List<bool>> dp = [];
	int i = 0;
	while ((i <= m)) {
		List<bool> row = [];
		int j = 0;
		while ((j <= n)) {
			row = (row + [false]);
			j = ((j + 1)).toInt();
		}
		dp = (dp + [row]);
		i = ((i + 1)).toInt();
	}
	dp[m][n] = true;
	int i2 = m;
	while ((i2 >= 0)) {
		int j2 = (n - 1);
		while ((j2 >= 0)) {
			bool first = false;
			if ((i2 < m)) {
				if ((((_indexString(p, j2) == _indexString(s, i2))) || ((_indexString(p, j2) == ".")))) {
					first = true;
				}
			}
			bool star = false;
			if (((j2 + 1) < n)) {
				if ((_indexString(p, ((j2 + 1)).toInt()) == "*")) {
					star = true;
				}
			}
			if (star) {
				bool ok = false;
				if (dp[i2][(j2 + 2)]) {
					ok = true;
				} else {
					if (first) {
						if (dp[(i2 + 1)][j2]) {
							ok = true;
						}
					}
				}
				dp[i2][j2] = ok;
			} else {
				bool ok = false;
				if (first) {
					if (dp[(i2 + 1)][(j2 + 1)]) {
						ok = true;
					}
				}
				dp[i2][j2] = ok;
			}
			j2 = ((j2 - 1)).toInt();
		}
		i2 = ((i2 - 1)).toInt();
	}
	return dp[0][0];
}

void test_example_1() {
	if (!((isMatch("aa", "a") == false))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((isMatch("aa", "a*") == true))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!((isMatch("ab", ".*") == true))) { throw Exception('expect failed'); }
}

void test_example_4() {
	if (!((isMatch("aab", "c*a*b") == true))) { throw Exception('expect failed'); }
}

void test_example_5() {
	if (!((isMatch("mississippi", "mis*is*p*.") == false))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
	if (!_runTest("example 4", test_example_4)) failures++;
	if (!_runTest("example 5", test_example_5)) failures++;
	if (failures > 0) {
		print("\n[FAIL] $failures test(s) failed.");
	}
}

String _formatDuration(Duration d) {
    if (d.inMicroseconds < 1000) return '${d.inMicroseconds}µs';
    if (d.inMilliseconds < 1000) return '${d.inMilliseconds}ms';
    return '${(d.inMilliseconds/1000).toStringAsFixed(2)}s';
}

String _indexString(String s, int i) {
    var runes = s.runes.toList();
    if (i < 0) {
        i += runes.length;
    }
    if (i < 0 || i >= runes.length) {
        throw RangeError('index out of range');
    }
    return String.fromCharCode(runes[i]);
}

bool _runTest(String name, void Function() f) {
    stdout.write('   test $name ...');
    var start = DateTime.now();
    try {
        f();
        var d = DateTime.now().difference(start);
        stdout.writeln(' ok (${_formatDuration(d)})');
        return true;
    } catch (e) {
        var d = DateTime.now().difference(start);
        stdout.writeln(' fail $e (${_formatDuration(d)})');
        return false;
    }
}


