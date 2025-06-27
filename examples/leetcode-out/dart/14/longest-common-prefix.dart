import 'dart:io';

String longestCommonPrefix(List<String> strs) {
	if ((strs.length == 0)) {
		return "";
	}
	String prefix = strs[0];
	for (var i = 1; i < strs.length; i++) {
		int j = 0;
		String current = strs[i];
		while (((j < prefix.length) && (j < current.length))) {
			if ((_indexString(prefix, j) != _indexString(current, j))) {
				break;
			}
			j = ((j + 1)).toInt();
		}
		prefix = prefix.substring(0, j);
		if ((prefix == "")) {
			break;
		}
	}
	return prefix;
}

void test_example_1() {
	if (!((longestCommonPrefix(["flower", "flow", "flight"]) == "fl"))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!((longestCommonPrefix(["dog", "racecar", "car"]) == ""))) { throw Exception('expect failed'); }
}

void test_single_string() {
	if (!((longestCommonPrefix(["single"]) == "single"))) { throw Exception('expect failed'); }
}

void test_no_common_prefix() {
	if (!((longestCommonPrefix(["a", "b", "c"]) == ""))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("single string", test_single_string)) failures++;
	if (!_runTest("no common prefix", test_no_common_prefix)) failures++;
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


