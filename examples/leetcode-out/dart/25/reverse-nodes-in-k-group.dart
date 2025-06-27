import 'dart:io';

List<int> reverseKGroup(List<int> nums, int k) {
	int n = nums.length;
	if ((k <= 1)) {
		return nums;
	}
	List result = [];
	int i = 0;
	while ((i < n)) {
		int end = (i + k);
		if ((end <= n)) {
			int j = (end - 1);
			while ((j >= i)) {
				result = (result + [nums[j]]);
				j = ((j - 1)).toInt();
			}
		} else {
			int j = i;
			while ((j < n)) {
				result = (result + [nums[j]]);
				j = ((j + 1)).toInt();
			}
		}
		i = ((i + k)).toInt();
	}
	return result;
}

void test_example_1() {
	if (!(_equal(reverseKGroup([1, 2, 3, 4, 5], 2), [2, 1, 4, 3, 5]))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!(_equal(reverseKGroup([1, 2, 3, 4, 5], 3), [3, 2, 1, 4, 5]))) { throw Exception('expect failed'); }
}

void test_k_equals_list_length() {
	if (!(_equal(reverseKGroup([1, 2, 3, 4], 4), [4, 3, 2, 1]))) { throw Exception('expect failed'); }
}

void test_k_greater_than_length() {
	if (!(_equal(reverseKGroup([1, 2, 3], 5), [1, 2, 3]))) { throw Exception('expect failed'); }
}

void test_k_is_one() {
	if (!(_equal(reverseKGroup([1, 2, 3], 1), [1, 2, 3]))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("k equals list length", test_k_equals_list_length)) failures++;
	if (!_runTest("k greater than length", test_k_greater_than_length)) failures++;
	if (!_runTest("k is one", test_k_is_one)) failures++;
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


