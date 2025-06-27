import 'dart:io';

List<String> generateParenthesis(int n) {
	List<String> result = [];
	void backtrack(String current, int open, int close) {
		if ((current.length == (n * 2))) {
			result = (result + [current]);
		} else {
			if ((open < n)) {
				backtrack((current + "("), (open + 1), close);
			}
			if ((close < open)) {
				backtrack((current + ")"), open, (close + 1));
			}
		}
	}
	backtrack("", 0, 0);
	return result;
}

void test_example_1() {
	if (!(_equal(generateParenthesis(3), ["((()))", "(()())", "(())()", "()(())", "()()()"]))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!(_equal(generateParenthesis(1), ["()"]))) { throw Exception('expect failed'); }
}

void test_two_pairs() {
	if (!(_equal(generateParenthesis(2), ["(())", "()()"]))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("two pairs", test_two_pairs)) failures++;
	if (failures > 0) {
		print("\n[FAIL] $failures test(s) failed.");
	}
}

bool _equal(dynamic a, dynamic b) {
    if (a is List && b is List) {
        if (a.length != b.length) return false;
        for (var i = 0; i < a.length; i++) { if (!_equal(a[i], b[i])) return false; }
        return true;
    }
    if (a is Map && b is Map) {
        if (a.length != b.length) return false;
        for (var k in a.keys) { if (!b.containsKey(k) || !_equal(a[k], b[k])) return false; }
        return true;
    }
    return a == b;
}

String _formatDuration(Duration d) {
    if (d.inMicroseconds < 1000) return '${d.inMicroseconds}µs';
    if (d.inMilliseconds < 1000) return '${d.inMilliseconds}ms';
    return '${(d.inMilliseconds/1000).toStringAsFixed(2)}s';
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


