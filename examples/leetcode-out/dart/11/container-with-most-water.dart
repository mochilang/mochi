import 'dart:io';

int maxArea(List<int> height) {
	int left = 0;
	int right = (height.length - 1);
	dynamic maxArea = 0;
	while ((left < right)) {
		int width = (right - left);
		int h = 0;
		if ((height[left] < height[right])) {
			h = (height[left]).toInt();
		} else {
			h = (height[right]).toInt();
		}
		int area = (h * width);
		if ((area > maxArea)) {
			maxArea = area;
		}
		if ((height[left] < height[right])) {
			left = ((left + 1)).toInt();
		} else {
			right = ((right - 1)).toInt();
		}
	}
	return maxArea;
}

void test_example_1() {
	if (!((maxArea([1, 8, 6, 2, 5, 4, 8, 3, 7]) == 49))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((maxArea([1, 1]) == 1))) { throw Exception('expect failed'); }
}

void test_decreasing_heights() {
	if (!((maxArea([4, 3, 2, 1, 4]) == 16))) { throw Exception('expect failed'); }
}

void test_short_array() {
	if (!((maxArea([1, 2, 1]) == 2))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("decreasing heights", test_decreasing_heights)) failures++;
	if (!_runTest("short array", test_short_array)) failures++;
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


