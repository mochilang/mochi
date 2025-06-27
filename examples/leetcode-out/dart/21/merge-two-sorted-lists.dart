import 'dart:io';

List<int> mergeTwoLists(List<int> l1, List<int> l2) {
	int i = 0;
	int j = 0;
	List result = [];
	while (((i < l1.length) && (j < l2.length))) {
		if ((l1[i] <= l2[j])) {
			result = (result + [l1[i]]);
			i = ((i + 1)).toInt();
		} else {
			result = (result + [l2[j]]);
			j = ((j + 1)).toInt();
		}
	}
	while ((i < l1.length)) {
		result = (result + [l1[i]]);
		i = ((i + 1)).toInt();
	}
	while ((j < l2.length)) {
		result = (result + [l2[j]]);
		j = ((j + 1)).toInt();
	}
	return result;
}

void test_example_1() {
	if (!(_equal(mergeTwoLists([1, 2, 4], [1, 3, 4]), [1, 1, 2, 3, 4, 4]))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!(_equal(mergeTwoLists([], []), []))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!(_equal(mergeTwoLists([], [0]), [0]))) { throw Exception('expect failed'); }
}

void test_different_lengths() {
	if (!(_equal(mergeTwoLists([1, 5, 7], [2, 3, 4, 6, 8]), [1, 2, 3, 4, 5, 6, 7, 8]))) { throw Exception('expect failed'); }
}

void test_one_list_empty() {
	if (!(_equal(mergeTwoLists([1, 2, 3], []), [1, 2, 3]))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
	if (!_runTest("different lengths", test_different_lengths)) failures++;
	if (!_runTest("one list empty", test_one_list_empty)) failures++;
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


