dynamic convert(s, numRows) {
	if (((numRows <= 1) || (numRows >= s.length))) {
		return s;
	}
	dynamic rows = [];
	dynamic i = 0;
	while ((i < numRows)) {
		rows = (rows + [""]);
		i = (i + 1);
	}
	dynamic curr = 0;
	dynamic step = 1;
	for (var ch in s) {
		rows[curr] = (rows[curr] + ch);
		if ((curr == 0)) {
			step = 1;
		} else 
		if ((curr == (numRows - 1))) {
			step = -1;
		}
		curr = (curr + step);
	}
	dynamic result = "";
	for (var row in rows) {
		result = (result + row);
	}
	return result;
}

void main() {
}
