import 'dart:io';

int digit(String ch) {
	if ((ch == "0")) {
		return 0;
	}
	if ((ch == "1")) {
		return 1;
	}
	if ((ch == "2")) {
		return 2;
	}
	if ((ch == "3")) {
		return 3;
	}
	if ((ch == "4")) {
		return 4;
	}
	if ((ch == "5")) {
		return 5;
	}
	if ((ch == "6")) {
		return 6;
	}
	if ((ch == "7")) {
		return 7;
	}
	if ((ch == "8")) {
		return 8;
	}
	if ((ch == "9")) {
		return 9;
	}
	return -1;
}

int myAtoi(String s) {
	int i = 0;
	int n = s.length;
	while (((i < n) && (_indexString(s, i) == _indexString(" ", 0)))) {
		i = ((i + 1)).toInt();
	}
	int sign = 1;
	if (((i < n) && (((_indexString(s, i) == _indexString("+", 0)) || (_indexString(s, i) == _indexString("-", 0)))))) {
		if ((_indexString(s, i) == _indexString("-", 0))) {
			sign = (-1).toInt();
		}
		i = ((i + 1)).toInt();
	}
	int result = 0;
	while ((i < n)) {
		String ch = s.substring(i, (i + 1));
		int d = digit(ch);
		if ((d < 0)) {
			break;
		}
		result = (((result * 10) + d)).toInt();
		i = ((i + 1)).toInt();
	}
	result = ((result * sign)).toInt();
	if ((result > 2147483647)) {
		return 2147483647;
	}
	if ((result < (-2147483648))) {
		return -2147483648;
	}
	return result;
}

void test_example_1() {
	if (!((myAtoi("42") == 42))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((myAtoi("   -42") == (-42)))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!((myAtoi("4193 with words") == 4193))) { throw Exception('expect failed'); }
}

void test_example_4() {
	if (!((myAtoi("words and 987") == 0))) { throw Exception('expect failed'); }
}

void test_example_5() {
	if (!((myAtoi("-91283472332") == (-2147483648)))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
	if (!_runTest("example 4", test_example_4)) failures++;
	if (!_runTest("example 5", test_example_5)) failures++;
	if (failures > 0) {
		print("\n[FAIL] $failures test(s) failed.");
	}
}

String _formatDuration(Duration d) {
    if (d.inMicroseconds < 1000) return '${d.inMicroseconds}µs';
    if (d.inMilliseconds < 1000) return '${d.inMilliseconds}ms';
    return '${(d.inMilliseconds/1000).toStringAsFixed(2)}s';
}

String _indexString(String s, int i) {
    var runes = s.runes.toList();
    if (i < 0) {
        i += runes.length;
    }
    if (i < 0 || i >= runes.length) {
        throw RangeError('index out of range');
    }
    return String.fromCharCode(runes[i]);
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


