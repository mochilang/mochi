import 'dart:io';

int romanToInt(String s) {
	Map<String, int> values = {"I": 1, "V": 5, "X": 10, "L": 50, "C": 100, "D": 500, "M": 1000};
	int total = 0;
	int i = 0;
	int n = s.length;
	while ((i < n)) {
		int curr = values[_indexString(s, i)];
		if (((i + 1) < n)) {
			int next = values[_indexString(s, ((i + 1)).toInt())];
			if ((curr < next)) {
				total = (((total + next) - curr)).toInt();
				i = ((i + 2)).toInt();
				continue;
			}
		}
		total = ((total + curr)).toInt();
		i = ((i + 1)).toInt();
	}
	return total;
}

void test_example_1() {
	if (!((romanToInt("III") == 3))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((romanToInt("LVIII") == 58))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!((romanToInt("MCMXCIV") == 1994))) { throw Exception('expect failed'); }
}

void test_subtractive() {
	if (!((romanToInt("IV") == 4))) { throw Exception('expect failed'); }
	if (!((romanToInt("IX") == 9))) { throw Exception('expect failed'); }
}

void test_tens() {
	if (!((romanToInt("XL") == 40))) { throw Exception('expect failed'); }
	if (!((romanToInt("XC") == 90))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
	if (!_runTest("subtractive", test_subtractive)) failures++;
	if (!_runTest("tens", test_tens)) failures++;
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


