int lengthOfLongestSubstring(String s) {
	dynamic n = s.length;
	dynamic start = 0;
	dynamic best = 0;
	dynamic i = 0;
	while ((i < n)) {
		dynamic j = start;
		while ((j < i)) {
			if ((_indexString(s, j) == _indexString(s, i))) {
				start = ((j + 1)).toInt();
				break;
			}
			j = ((j + 1)).toInt();
		}
		dynamic length = ((i - start) + 1);
		if ((length > best)) {
			best = length;
		}
		i = ((i + 1)).toInt();
	}
	return best;
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

