import 'dart:io';

int expand(String s, int left, int right) {
	int l = left;
	int r = right;
	int n = s.length;
	while (((l >= 0) && (r < n))) {
		if ((_indexString(s, l) != _indexString(s, r))) {
			break;
		}
		l = ((l - 1)).toInt();
		r = ((r + 1)).toInt();
	}
	return ((r - l) - 1);
}

String longestPalindrome(String s) {
	if ((s.length <= 1)) {
		return s;
	}
	int start = 0;
	int end = 0;
	int n = s.length;
	for (var i = 0; i < n; i++) {
		int len1 = expand(s, i, i);
		int len2 = expand(s, i, (i + 1));
		int l = len1;
		if ((len2 > len1)) {
			l = len2;
		}
		if ((l > ((end - start)))) {
			start = ((i - ((((l - 1)) ~/ 2)))).toInt();
			end = ((i + ((l ~/ 2)))).toInt();
		}
	}
	return s.substring(start, (end + 1));
}

void test_example_1() {
	String ans = longestPalindrome("babad");
	if (!(((ans == "bab") || (ans == "aba")))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((longestPalindrome("cbbd") == "bb"))) { throw Exception('expect failed'); }
}

void test_single_char() {
	if (!((longestPalindrome("a") == "a"))) { throw Exception('expect failed'); }
}

void test_two_chars() {
	String ans = longestPalindrome("ac");
	if (!(((ans == "a") || (ans == "c")))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("single char", test_single_char)) failures++;
	if (!_runTest("two chars", test_two_chars)) failures++;
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


