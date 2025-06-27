import 'dart:io';

List<int> swapPairs(List<int> nums) {
	int i = 0;
	List result = [];
	while ((i < nums.length)) {
		if (((i + 1) < nums.length)) {
			result = (result + [nums[(i + 1)], nums[i]]);
		} else {
			result = (result + [nums[i]]);
		}
		i = ((i + 2)).toInt();
	}
	return result;
}

void test_example_1() {
	if (!(_equal(swapPairs([1, 2, 3, 4]), [2, 1, 4, 3]))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!(_equal(swapPairs([]), []))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!(_equal(swapPairs([1]), [1]))) { throw Exception('expect failed'); }
}

void test_odd_length() {
	if (!(_equal(swapPairs([1, 2, 3]), [2, 1, 3]))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
	if (!_runTest("odd length", test_odd_length)) failures++;
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


