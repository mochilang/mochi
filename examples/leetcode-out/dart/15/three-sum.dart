import 'dart:io';

List<List<int>> threeSum(List<int> nums) {
	List<int> sorted = (() {
	var _res = [];
	for (var x in nums) {
		_res.add(x);
	}
	var items = List.from(_res);
	items.sort((xA, xB) {
		var x = xA;
		var keyA = x;
		x = xB;
		var keyB = x;
		return Comparable.compare(keyA, keyB);
	});
	_res = items;
	return _res;
})();
	int n = sorted.length;
	List<List<int>> res = [];
	int i = 0;
	while ((i < n)) {
		if (((i > 0) && (sorted[i] == sorted[(i - 1)]))) {
			i = ((i + 1)).toInt();
			continue;
		}
		int left = (i + 1);
		int right = (n - 1);
		while ((left < right)) {
			dynamic sum = ((sorted[i] + sorted[left]) + sorted[right]);
			if ((sum == 0)) {
				res = (res + [[sorted[i], sorted[left], sorted[right]]]);
				left = ((left + 1)).toInt();
				while (((left < right) && (sorted[left] == sorted[(left - 1)]))) {
					left = ((left + 1)).toInt();
				}
				right = ((right - 1)).toInt();
				while (((left < right) && (sorted[right] == sorted[(right + 1)]))) {
					right = ((right - 1)).toInt();
				}
			} else 
			if ((sum < 0)) {
				left = ((left + 1)).toInt();
			} else {
				right = ((right - 1)).toInt();
			}
		}
		i = ((i + 1)).toInt();
	}
	return res;
}

void test_example_1() {
	if (!(_equal(threeSum([-1, 0, 1, 2, -1, -4]), [[-1, -1, 2], [-1, 0, 1]]))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!(_equal(threeSum([0, 1, 1]), []))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!(_equal(threeSum([0, 0, 0]), [[0, 0, 0]]))) { throw Exception('expect failed'); }
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


