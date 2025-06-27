import 'dart:io';

List<int> mergeKLists(List<List<int>> lists) {
	int k = lists.length;
	List<int> indices = [];
	int i = 0;
	while ((i < k)) {
		indices = (indices + [0]);
		i = ((i + 1)).toInt();
	}
	List<int> result = [];
	while (true) {
		int best = 0;
		int bestList = -1;
		bool found = false;
		int j = 0;
		while ((j < k)) {
			int idx = indices[j];
			if ((idx < lists[j].length)) {
				int val = lists[j][idx];
				if ((!found || (val < best))) {
					best = val;
					bestList = j;
					found = true;
				}
			}
			j = ((j + 1)).toInt();
		}
		if (!found) {
			break;
		}
		result = (result + [best]);
		indices[bestList] = (indices[bestList] + 1);
	}
	return result;
}

void test_example_1() {
	if (!(_equal(mergeKLists([[1, 4, 5], [1, 3, 4], [2, 6]]), [1, 1, 2, 3, 4, 4, 5, 6]))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!(_equal(mergeKLists([]), []))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!(_equal(mergeKLists([[]]), []))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
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


