int digit(String ch) {
	if ((ch == "0")) {
		return 0;
	}
	if ((ch == "1")) {
		return 1;
	}
	if ((ch == "2")) {
		return 2;
	}
	if ((ch == "3")) {
		return 3;
	}
	if ((ch == "4")) {
		return 4;
	}
	if ((ch == "5")) {
		return 5;
	}
	if ((ch == "6")) {
		return 6;
	}
	if ((ch == "7")) {
		return 7;
	}
	if ((ch == "8")) {
		return 8;
	}
	if ((ch == "9")) {
		return 9;
	}
	return -1;
}

int myAtoi(String s) {
	int i = 0;
	int n = s.length;
	while (((i < n) && (_indexString(s, i) == _indexString(" ", 0)))) {
		i = (i + 1);
	}
	int sign = 1;
	if (((i < n) && (((_indexString(s, i) == _indexString("+", 0)) || (_indexString(s, i) == _indexString("-", 0)))))) {
		if ((_indexString(s, i) == _indexString("-", 0))) {
			sign = -1;
		}
		i = (i + 1);
	}
	int result = 0;
	while ((i < n)) {
		String ch = s.substring(i, (i + 1));
		int d = digit(ch);
		if ((d < 0)) {
			break;
		}
		result = ((result * 10) + d);
		i = (i + 1);
	}
	result = (result * sign);
	if ((result > 2147483647)) {
		return 2147483647;
	}
	if ((result < (-2147483648))) {
		return -2147483648;
	}
	return result;
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
