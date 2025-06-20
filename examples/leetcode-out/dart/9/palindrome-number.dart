bool isPalindrome(int x) {
	if ((x < 0)) {
		return false;
	}
	dynamic s = x.toString();
	dynamic n = s.length;
	for (var i = 0; i < (n ~/ 2); i++) {
		if ((_indexString(s, (i).toInt()) != _indexString(s, (((n - 1) - i)).toInt()))) {
			return false;
		}
	}
	return true;
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

