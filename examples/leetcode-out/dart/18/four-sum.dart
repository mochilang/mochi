import 'dart:io';

List<List<int>> fourSum(List<int> nums, int target) {
	List<int> sorted = (() {
	var _res = [];
	for (var n in nums) {
		_res.add(n);
	}
	var items = List.from(_res);
	items.sort((nA, nB) {
		var n = nA;
		var keyA = n;
		n = nB;
		var keyB = n;
		return Comparable.compare(keyA, keyB);
	});
	_res = items;
	return _res;
})();
	int n = sorted.length;
	List<List<int>> result = [];
	for (var i = 0; i < n; i++) {
		if (((i > 0) && (sorted[i] == sorted[(i - 1)]))) {
			continue;
		}
		for (var j = (i + 1); j < n; j++) {
			if (((j > (i + 1)) && (sorted[j] == sorted[(j - 1)]))) {
				continue;
			}
			dynamic left = (j + 1);
			int right = (n - 1);
			while ((left < right)) {
				dynamic sum = (((sorted[i] + sorted[j]) + sorted[left]) + sorted[right]);
				if ((sum == target)) {
					result = (result + [[sorted[i], sorted[j], sorted[left], sorted[right]]]);
					left = (left + 1);
					right = ((right - 1)).toInt();
					while (((left < right) && (sorted[left] == sorted[(left - 1)]))) {
						left = (left + 1);
					}
					while (((left < right) && (sorted[right] == sorted[(right + 1)]))) {
						right = ((right - 1)).toInt();
					}
				} else 
				if ((sum < target)) {
					left = (left + 1);
				} else {
					right = ((right - 1)).toInt();
				}
			}
		}
	}
	return result;
}

void test_example_1() {
	if (!(_equal(fourSum([1, 0, -1, 0, -2, 2], 0), [[-2, -1, 1, 2], [-2, 0, 0, 2], [-1, 0, 0, 1]]))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!(_equal(fourSum([2, 2, 2, 2, 2], 8), [[2, 2, 2, 2]]))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
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


