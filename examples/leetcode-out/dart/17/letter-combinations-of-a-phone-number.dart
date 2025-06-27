import 'dart:io';

List<String> letterCombinations(String digits) {
	if ((digits.length == 0)) {
		return [];
	}
	Map<String, List<String>> mapping = {"2": ["a", "b", "c"], "3": ["d", "e", "f"], "4": ["g", "h", "i"], "5": ["j", "k", "l"], "6": ["m", "n", "o"], "7": ["p", "q", "r", "s"], "8": ["t", "u", "v"], "9": ["w", "x", "y", "z"]};
	List<String> result = [""];
	var _tmp0 = digits;
	for (var _tmp1 in _tmp0.runes) {
		var d = String.fromCharCode(_tmp1);
		if (!((mapping.containsKey(d)))) {
			continue;
		}
		List<String> letters = mapping[d];
		List<String> next = (() {
	var _res = [];
	for (var p in result) {
		for (var ch in letters) {
			_res.add((p + ch));
		}
	}
	return _res;
})();
		result = next;
	}
	return result;
}

void test_example_1() {
	if (!(_equal(letterCombinations("23"), ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"]))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!(_equal(letterCombinations(""), []))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!(_equal(letterCombinations("2"), ["a", "b", "c"]))) { throw Exception('expect failed'); }
}

void test_single_seven() {
	if (!(_equal(letterCombinations("7"), ["p", "q", "r", "s"]))) { throw Exception('expect failed'); }
}

void test_mix() {
	if (!(_equal(letterCombinations("79"), ["pw", "px", "py", "pz", "qw", "qx", "qy", "qz", "rw", "rx", "ry", "rz", "sw", "sx", "sy", "sz"]))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
	if (!_runTest("single seven", test_single_seven)) failures++;
	if (!_runTest("mix", test_mix)) failures++;
	if (failures > 0) {
		print("\n[FAIL] $failures test(s) failed.");
	}
}

bool _equal(dynamic a, dynamic b) {
    if (a is List && b is List) {
        if (a.length != b.length) return false;
        for (var i = 0; i < a.length; i++) { if (!_equal(a[i], b[i])) return false; }
        return true;
    }
    if (a is Map && b is Map) {
        if (a.length != b.length) return false;
        for (var k in a.keys) { if (!b.containsKey(k) || !_equal(a[k], b[k])) return false; }
        return true;
    }
    return a == b;
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


