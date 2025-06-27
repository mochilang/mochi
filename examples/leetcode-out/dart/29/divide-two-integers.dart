import 'dart:io';

int divide(int dividend, int divisor) {
	if (((dividend == ((-2147483647 - 1))) && (divisor == (-1)))) {
		return 2147483647;
	}
	bool negative = false;
	if ((dividend < 0)) {
		negative = !negative;
		dividend = (-dividend).toInt();
	}
	if ((divisor < 0)) {
		negative = !negative;
		divisor = (-divisor).toInt();
	}
	int quotient = 0;
	while ((dividend >= divisor)) {
		int temp = divisor;
		int multiple = 1;
		while ((dividend >= (temp + temp))) {
			temp = ((temp + temp)).toInt();
			multiple = ((multiple + multiple)).toInt();
		}
		dividend = ((dividend - temp)).toInt();
		quotient = ((quotient + multiple)).toInt();
	}
	if (negative) {
		quotient = (-quotient).toInt();
	}
	if ((quotient > 2147483647)) {
		return 2147483647;
	}
	if ((quotient < ((-2147483647 - 1)))) {
		return -2147483648;
	}
	return quotient;
}

void test_example_1() {
	if (!((divide(10, 3) == 3))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((divide(7, -3) == (-2)))) { throw Exception('expect failed'); }
}

void test_overflow() {
	if (!((divide(-2147483648, -1) == 2147483647))) { throw Exception('expect failed'); }
}

void test_divide_by_1() {
	if (!((divide(12345, 1) == 12345))) { throw Exception('expect failed'); }
}

void test_negative_result() {
	if (!((divide(-15, 2) == (-7)))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("overflow", test_overflow)) failures++;
	if (!_runTest("divide by 1", test_divide_by_1)) failures++;
	if (!_runTest("negative result", test_negative_result)) failures++;
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


