import 'dart:io';

int removeDuplicates(List<int> nums) {
	if ((nums.length == 0)) {
		return 0;
	}
	dynamic count = 1;
	int prev = nums[0];
	int i = 1;
	while ((i < nums.length)) {
		int cur = nums[i];
		if ((cur != prev)) {
			count = (count + 1);
			prev = cur;
		}
		i = ((i + 1)).toInt();
	}
	return count;
}

void test_example_1() {
	if (!((removeDuplicates([1, 1, 2]) == 2))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((removeDuplicates([0, 0, 1, 1, 1, 2, 2, 3, 3, 4]) == 5))) { throw Exception('expect failed'); }
}

void test_empty() {
	if (!((removeDuplicates([]) == 0))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("empty", test_empty)) failures++;
	if (failures > 0) {
		print("\n[FAIL] $failures test(s) failed.");
	}
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


