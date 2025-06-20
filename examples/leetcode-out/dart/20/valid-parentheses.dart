bool isValid(String s) {
	dynamic stack = [];
	dynamic n = s.length;
	for (var i = 0; i < n; i++) {
		dynamic c = _indexString(s, (i).toInt());
		if ((c == "(")) {
			stack = (stack + [")"]);
		} else 
		if ((c == "[")) {
			stack = (stack + ["]"]);
		} else 
		if ((c == "{")) {
			stack = (stack + ["}"]);
		} else {
			if ((stack.length == 0)) {
				return false;
			}
			dynamic top = stack[(stack.length - 1)];
			if ((top != c)) {
				return false;
			}
			stack = stack.sublist(0, (stack.length - 1));
		}
	}
	return (stack.length == 0);
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

