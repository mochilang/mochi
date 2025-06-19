String convert(String s, int numRows) {
	if (((numRows <= 1) || (numRows >= s.length))) {
		return s;
	}
	List<String> rows = [];
	int i = 0;
	while ((i < numRows)) {
		rows = (rows + [""]);
		i = (i + 1);
	}
	int curr = 0;
	int step = 1;
	var _tmp0 = s;
	for (var _tmp1 in _tmp0.runes) {
		var ch = String.fromCharCode(_tmp1);
		rows[curr] = (rows[curr] + ch);
		if ((curr == 0)) {
			step = 1;
		} else 
		if ((curr == (numRows - 1))) {
			step = -1;
		}
		curr = (curr + step);
	}
	String result = "";
	for (var row in rows) {
		result = (result + row);
	}
	return result;
}

void main() {
}
