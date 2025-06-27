import 'dart:io';

int removeElement(List<int> nums, int val) {
	int k = 0;
	int i = 0;
	while ((i < nums.length)) {
		if ((nums[i] != val)) {
			nums[k] = nums[i];
			k = ((k + 1)).toInt();
		}
		i = ((i + 1)).toInt();
	}
	return k;
}

void test_example_1() {
	List<int> nums = [3, 2, 2, 3];
	int k = removeElement(nums, 3);
	if (!((k == 2))) { throw Exception('expect failed'); }
	if (!(_equal(nums.sublist(0, k), [2, 2]))) { throw Exception('expect failed'); }
}

void test_example_2() {
	List<int> nums = [0, 1, 2, 2, 3, 0, 4, 2];
	int k = removeElement(nums, 2);
	if (!((k == 5))) { throw Exception('expect failed'); }
	if (!(_equal(nums.sublist(0, k), [0, 1, 3, 0, 4]))) { throw Exception('expect failed'); }
}

void test_no_removal() {
	List<int> nums = [1, 2, 3];
	int k = removeElement(nums, 4);
	if (!((k == 3))) { throw Exception('expect failed'); }
	if (!(_equal(nums.sublist(0, k), [1, 2, 3]))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("no removal", test_no_removal)) failures++;
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


