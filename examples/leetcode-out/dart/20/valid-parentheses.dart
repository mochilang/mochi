import 'dart:io';

bool isValid(String s) {
	List<String> stack = [];
	int n = s.length;
	for (var i = 0; i < n; i++) {
		String c = _indexString(s, (i).toInt());
		if ((c == "(")) {
			stack = (stack + [")"]);
		} else 
		if ((c == "[")) {
			stack = (stack + ["]"]);
		} else 
		if ((c == "{")) {
			stack = (stack + ["}"]);
		} else {
			if ((stack.length == 0)) {
				return false;
			}
			String top = stack[(stack.length - 1)];
			if ((top != c)) {
				return false;
			}
			stack = stack.sublist(0, (stack.length - 1));
		}
	}
	return (stack.length == 0);
}

void test_example_1() {
	if (!((isValid("()") == true))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((isValid("()[]{}") == true))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!((isValid("(]") == false))) { throw Exception('expect failed'); }
}

void test_example_4() {
	if (!((isValid("([)]") == false))) { throw Exception('expect failed'); }
}

void test_example_5() {
	if (!((isValid("{[]}") == true))) { throw Exception('expect failed'); }
}

void test_empty_string() {
	if (!((isValid("") == true))) { throw Exception('expect failed'); }
}

void test_single_closing() {
	if (!((isValid("]") == false))) { throw Exception('expect failed'); }
}

void test_unmatched_open() {
	if (!((isValid("((") == false))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
	if (!_runTest("example 4", test_example_4)) failures++;
	if (!_runTest("example 5", test_example_5)) failures++;
	if (!_runTest("empty string", test_empty_string)) failures++;
	if (!_runTest("single closing", test_single_closing)) failures++;
	if (!_runTest("unmatched open", test_unmatched_open)) failures++;
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


