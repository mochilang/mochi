import 'dart:io';

List<int> addTwoNumbers(List<int> l1, List<int> l2) {
	int i = 0;
	int j = 0;
	int carry = 0;
	List<int> result = [];
	while ((((i < l1.length) || (j < l2.length)) || (carry > 0))) {
		int x = 0;
		if ((i < l1.length)) {
			x = (l1[i]).toInt();
			i = ((i + 1)).toInt();
		}
		int y = 0;
		if ((j < l2.length)) {
			y = (l2[j]).toInt();
			j = ((j + 1)).toInt();
		}
		dynamic sum = ((x + y) + carry);
		dynamic digit = (sum % 10);
		carry = ((sum ~/ 10)).toInt();
		result = (result + [digit]);
	}
	return result;
}

void test_example_1() {
	if (!(_equal(addTwoNumbers([2, 4, 3], [5, 6, 4]), [7, 0, 8]))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!(_equal(addTwoNumbers([0], [0]), [0]))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!(_equal(addTwoNumbers([9, 9, 9, 9, 9, 9, 9], [9, 9, 9, 9]), [8, 9, 9, 9, 0, 0, 0, 1]))) { throw Exception('expect failed'); }
}

void main() {
	int failures = 0;
	if (!_runTest("example 1", test_example_1)) failures++;
	if (!_runTest("example 2", test_example_2)) failures++;
	if (!_runTest("example 3", test_example_3)) failures++;
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


