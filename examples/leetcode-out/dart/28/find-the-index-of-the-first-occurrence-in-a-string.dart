int strStr(String haystack, String needle) {
	dynamic n = haystack.length;
	dynamic m = needle.length;
	if ((m == 0)) {
		return 0;
	}
	if ((m > n)) {
		return -1;
	}
	for (var i = 0; i < ((n - m) + 1); i++) {
		dynamic j = 0;
		while ((j < m)) {
			if ((_indexString(haystack, ((i + j)).toInt()) != _indexString(needle, j))) {
				break;
			}
			j = ((j + 1)).toInt();
		}
		if ((j == m)) {
			return i;
		}
	}
	return -1;
}

void main() {
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

