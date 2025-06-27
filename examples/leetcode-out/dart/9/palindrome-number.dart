import 'dart:io';

bool isPalindrome(int x) {
	if ((x < 0)) {
		return false;
	}
	String s = x.toString();
	int n = s.length;
	for (var i = 0; i < (n ~/ 2); i++) {
		if ((_indexString(s, (i).toInt()) != _indexString(s, (((n - 1) - i)).toInt()))) {
			return false;
		}
	}
	return true;
}

void test_example_1() {
	if (!((isPalindrome(121) == true))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((isPalindrome(-121) == false))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!((isPalindrome(10) == false))) { throw Exception('expect failed'); }
}

void test_zero() {
	if (!((isPalindrome(0) == true))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
	if (!_runTest("zero", test_zero)) failures++;
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


