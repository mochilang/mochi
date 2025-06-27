import 'dart:io';

int threeSumClosest(List<int> nums, int target) {
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
	int best = ((sorted[0] + sorted[1]) + sorted[2]);
	for (var i = 0; i < n; i++) {
		dynamic left = (i + 1);
		int right = (n - 1);
		while ((left < right)) {
			dynamic sum = ((sorted[i] + sorted[left]) + sorted[right]);
			if ((sum == target)) {
				return target;
			}
			int diff = 0;
			if ((sum > target)) {
				diff = ((sum - target)).toInt();
			} else {
				diff = ((target - sum)).toInt();
			}
			int bestDiff = 0;
			if ((best > target)) {
				bestDiff = ((best - target)).toInt();
			} else {
				bestDiff = ((target - best)).toInt();
			}
			if ((diff < bestDiff)) {
				best = (sum).toInt();
			}
			if ((sum < target)) {
				left = (left + 1);
			} else {
				right = ((right - 1)).toInt();
			}
		}
	}
	return best;
}

void test_example_1() {
	if (!((threeSumClosest([-1, 2, 1, -4], 1) == 2))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((threeSumClosest([0, 0, 0], 1) == 0))) { throw Exception('expect failed'); }
}

void test_additional() {
	if (!((threeSumClosest([1, 1, 1, 0], -100) == 2))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("additional", test_additional)) failures++;
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


