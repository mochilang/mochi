import 'dart:io';

int lengthOfLongestSubstring(String s) {
	int n = s.length;
	int start = 0;
	int best = 0;
	int i = 0;
	while ((i < n)) {
		int j = start;
		while ((j < i)) {
			if ((_indexString(s, j) == _indexString(s, i))) {
				start = ((j + 1)).toInt();
				break;
			}
			j = ((j + 1)).toInt();
		}
		int length = ((i - start) + 1);
		if ((length > best)) {
			best = length;
		}
		i = ((i + 1)).toInt();
	}
	return best;
}

void test_example_1() {
	if (!((lengthOfLongestSubstring("abcabcbb") == 3))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((lengthOfLongestSubstring("bbbbb") == 1))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!((lengthOfLongestSubstring("pwwkew") == 3))) { throw Exception('expect failed'); }
}

void test_empty_string() {
	if (!((lengthOfLongestSubstring("") == 0))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
	if (!_runTest("empty string", test_empty_string)) failures++;
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


