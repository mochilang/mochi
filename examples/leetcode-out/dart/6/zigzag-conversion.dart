import 'dart:io';

String convert(String s, int numRows) {
	if (((numRows <= 1) || (numRows >= s.length))) {
		return s;
	}
	List<String> rows = [];
	int i = 0;
	while ((i < numRows)) {
		rows = (rows + [""]);
		i = ((i + 1)).toInt();
	}
	int curr = 0;
	int step = 1;
	var _tmp0 = s;
	for (var _tmp1 in _tmp0.runes) {
		var ch = String.fromCharCode(_tmp1);
		rows[curr] = (rows[curr] + ch);
		if ((curr == 0)) {
			step = 1;
		} else 
		if ((curr == (numRows - 1))) {
			step = (-1).toInt();
		}
		curr = ((curr + step)).toInt();
	}
	String result = "";
	for (var row in rows) {
		result = (result + row);
	}
	return result;
}

void test_example_1() {
	if (!((convert("PAYPALISHIRING", 3) == "PAHNAPLSIIGYIR"))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((convert("PAYPALISHIRING", 4) == "PINALSIGYAHRPI"))) { throw Exception('expect failed'); }
}

void test_single_row() {
	if (!((convert("A", 1) == "A"))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("single row", test_single_row)) failures++;
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


