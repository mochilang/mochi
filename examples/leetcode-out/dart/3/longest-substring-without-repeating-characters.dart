int lengthOfLongestSubstring(String s) {
	int n = s.length;
	int start = 0;
	int best = 0;
	int i = 0;
	while ((i < n)) {
		var j = start;
		while ((j < i)) {
			if ((_indexString(s, j) == _indexString(s, i))) {
				start = (j + 1);
				break;
			}
			j = (j + 1);
		}
		var length = ((i - start) + 1);
		if ((length > best)) {
			best = length;
		}
		i = (i + 1);
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
