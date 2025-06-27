import 'dart:io';

String intToRoman(int num) {
	dynamic values = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
	List<String> symbols = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"];
	String result = "";
	int i = 0;
	while ((num > 0)) {
		while ((num >= values[i])) {
			result = (result + symbols[i]);
			num = ((num - values[i])).toInt();
		}
		i = ((i + 1)).toInt();
	}
	return result;
}

void test_example_1() {
	if (!((intToRoman(3) == "III"))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((intToRoman(58) == "LVIII"))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!((intToRoman(1994) == "MCMXCIV"))) { throw Exception('expect failed'); }
}

void test_small_numbers() {
	if (!((intToRoman(4) == "IV"))) { throw Exception('expect failed'); }
	if (!((intToRoman(9) == "IX"))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
	if (!_runTest("small numbers", test_small_numbers)) failures++;
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


