import 'dart:io';

List<int> removeNthFromEnd(List<int> nums, int n) {
	int idx = (nums.length - n);
	List result = [];
	int i = 0;
	while ((i < nums.length)) {
		if ((i != idx)) {
			result = (result + [nums[i]]);
		}
		i = ((i + 1)).toInt();
	}
	return result;
}

void test_example_1() {
	if (!(_equal(removeNthFromEnd([1, 2, 3, 4, 5], 2), [1, 2, 3, 5]))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!(_equal(removeNthFromEnd([1], 1), []))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!(_equal(removeNthFromEnd([1, 2], 1), [1]))) { throw Exception('expect failed'); }
}

void test_remove_first() {
	if (!(_equal(removeNthFromEnd([7, 8, 9], 3), [8, 9]))) { throw Exception('expect failed'); }
}

void test_remove_last() {
	if (!(_equal(removeNthFromEnd([7, 8, 9], 1), [7, 8]))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
	if (!_runTest("remove first", test_remove_first)) failures++;
	if (!_runTest("remove last", test_remove_last)) failures++;
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


