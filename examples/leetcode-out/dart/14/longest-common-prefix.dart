String longestCommonPrefix(strs) {
	if ((strs.length == 0)) {
		return "";
	}
	dynamic prefix = strs[0];
	for (var i = 1; i < strs.length; i++) {
		dynamic j = 0;
		dynamic current = strs[i];
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

