int expand(String s, int left, int right) {
	dynamic l = left;
	dynamic r = right;
	dynamic n = s.length;
	while (((l >= 0) && (r < n))) {
		if ((_indexString(s, l) != _indexString(s, r))) {
			break;
		}
		l = ((l - 1)).toInt();
		r = ((r + 1)).toInt();
	}
	return ((r - l) - 1);
}

String longestPalindrome(String s) {
	if ((s.length <= 1)) {
		return s;
	}
	dynamic start = 0;
	dynamic end = 0;
	dynamic n = s.length;
	for (var i = 0; i < n; i++) {
		dynamic len1 = expand(s, i, i);
		dynamic len2 = expand(s, i, (i + 1));
		dynamic l = len1;
		if ((len2 > len1)) {
			l = len2;
		}
		if ((l > ((end - start)))) {
			start = ((i - ((((l - 1)) ~/ 2)))).toInt();
			end = ((i + ((l ~/ 2)))).toInt();
		}
	}
	return s.substring(start, (end + 1));
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

