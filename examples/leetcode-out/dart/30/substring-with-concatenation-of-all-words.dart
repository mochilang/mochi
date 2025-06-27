import 'dart:io';

List<int> findSubstring(String s, List<String> words) {
	if ((words.length == 0)) {
		return [];
	}
	int wordLen = words[0].length;
	int wordCount = words.length;
	int totalLen = (wordLen * wordCount);
	if ((s.length < totalLen)) {
		return [];
	}
	Map<String, int> freq = {};
	for (var w in words) {
		if ((freq.containsKey(w))) {
			freq[w] = (freq[w] + 1);
		} else {
			freq[w] = 1;
		}
	}
	List<int> result = [];
	for (var offset = 0; offset < wordLen; offset++) {
		dynamic left = offset;
		dynamic count = 0;
		Map<String, int> seen = {};
		dynamic j = offset;
		while (((j + wordLen) <= s.length)) {
			String word = s.substring(j, (j + wordLen));
			j = (j + wordLen);
			if ((freq.containsKey(word))) {
				if ((seen.containsKey(word))) {
					seen[word] = (seen[word] + 1);
				} else {
					seen[word] = 1;
				}
				count = (count + 1);
				while ((seen[word] > freq[word])) {
					String lw = s.substring(left, (left + wordLen));
					seen[lw] = (seen[lw] - 1);
					left = (left + wordLen);
					count = (count - 1);
				}
				if ((count == wordCount)) {
					result = (result + [left]);
					String lw = s.substring(left, (left + wordLen));
					seen[lw] = (seen[lw] - 1);
					left = (left + wordLen);
					count = (count - 1);
				}
			} else {
				seen = {};
				count = 0;
				left = j;
			}
		}
	}
	return result;
}

void test_example_1() {
	if (!(_equal(findSubstring("barfoothefoobarman", ["foo", "bar"]), [0, 9]))) { throw Exception('expect failed'); }
}

void test_example_2() {
	if (!(_equal(findSubstring("wordgoodgoodgoodbestword", ["word", "good", "best", "word"]), []))) { throw Exception('expect failed'); }
}

void test_example_3() {
	if (!(_equal(findSubstring("barfoofoobarthefoobarman", ["bar", "foo", "the"]), [6, 9, 12]))) { throw Exception('expect failed'); }
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


