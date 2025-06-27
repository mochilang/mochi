import 'dart:io';

double findMedianSortedArrays(List<int> nums1, List<int> nums2) {
	List<int> merged = [];
	int i = 0;
	int j = 0;
	while (((i < nums1.length) || (j < nums2.length))) {
		if ((j >= nums2.length)) {
			merged = (merged + [nums1[i]]);
			i = ((i + 1)).toInt();
		} else 
		if ((i >= nums1.length)) {
			merged = (merged + [nums2[j]]);
			j = ((j + 1)).toInt();
		} else 
		if ((nums1[i] <= nums2[j])) {
			merged = (merged + [nums1[i]]);
			i = ((i + 1)).toInt();
		} else {
			merged = (merged + [nums2[j]]);
			j = ((j + 1)).toInt();
		}
	}
	int total = merged.length;
	if (((total % 2) == 1)) {
		return (merged[(total ~/ 2)]).toDouble();
	}
	int mid1 = merged[((total ~/ 2) - 1)];
	int mid2 = merged[(total ~/ 2)];
	return ((((mid1 + mid2))).toDouble() / 2);
}

void test_example_1() {
	if (!((findMedianSortedArrays([1, 3], [2]) == 2))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((findMedianSortedArrays([1, 2], [3, 4]) == 2.5))) { throw Exception('expect failed'); }
}

void test_empty_first() {
	if (!((findMedianSortedArrays(([] as List<int>), [1]) == 1))) { throw Exception('expect failed'); }
}

void test_empty_second() {
	if (!((findMedianSortedArrays([2], ([] as List<int>)) == 2))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("empty first", test_empty_first)) failures++;
	if (!_runTest("empty second", test_empty_second)) failures++;
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


