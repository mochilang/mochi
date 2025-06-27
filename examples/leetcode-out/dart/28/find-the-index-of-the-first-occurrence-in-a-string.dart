import 'dart:io';

int strStr(String haystack, String needle) {
	int n = haystack.length;
	int m = needle.length;
	if ((m == 0)) {
		return 0;
	}
	if ((m > n)) {
		return -1;
	}
	for (var i = 0; i < ((n - m) + 1); i++) {
		int j = 0;
		while ((j < m)) {
			if ((_indexString(haystack, ((i + j)).toInt()) != _indexString(needle, j))) {
				break;
			}
			j = ((j + 1)).toInt();
		}
		if ((j == m)) {
			return i;
		}
	}
	return -1;
}

void test_example_1() {
	if (!((strStr("sadbutsad", "sad") == 0))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((strStr("leetcode", "leeto") == (-1)))) { throw Exception('expect failed'); }
}

void test_empty_needle() {
	if (!((strStr("abc", "") == 0))) { throw Exception('expect failed'); }
}

void test_needle_at_end() {
	if (!((strStr("hello", "lo") == 3))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("empty needle", test_empty_needle)) failures++;
	if (!_runTest("needle at end", test_needle_at_end)) failures++;
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


