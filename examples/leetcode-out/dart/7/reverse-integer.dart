import 'dart:io';

int reverse(int x) {
	int sign = 1;
	int n = x;
	if ((n < 0)) {
		sign = (-1).toInt();
		n = (-n).toInt();
	}
	int rev = 0;
	while ((n != 0)) {
		int digit = (n % 10);
		rev = (((rev * 10) + digit)).toInt();
		n = ((n ~/ 10)).toInt();
	}
	rev = ((rev * sign)).toInt();
	if (((rev < ((-2147483647 - 1))) || (rev > 2147483647))) {
		return 0;
	}
	return rev;
}

void test_example_1() {
	if (!((reverse(123) == 321))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((reverse(-123) == (-321)))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!((reverse(120) == 21))) { throw Exception('expect failed'); }
}

void test_overflow() {
	if (!((reverse(1534236469) == 0))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
	if (!_runTest("overflow", test_overflow)) failures++;
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


