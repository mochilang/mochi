int romanToInt(String s) {
	dynamic values = {"I": 1, "V": 5, "X": 10, "L": 50, "C": 100, "D": 500, "M": 1000};
	dynamic total = 0;
	dynamic i = 0;
	dynamic n = s.length;
	while ((i < n)) {
		dynamic curr = values[_indexString(s, i)];
		if (((i + 1) < n)) {
			dynamic next = values[_indexString(s, ((i + 1)).toInt())];
			if ((curr < next)) {
				total = (((total + next) - curr)).toInt();
				i = ((i + 2)).toInt();
				continue;
			}
		}
		total = ((total + curr)).toInt();
		i = ((i + 1)).toInt();
	}
	return total;
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

