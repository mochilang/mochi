dynamic myAtoi(s) {
	dynamic i = 0;
	dynamic n = s.length;
	while (((i < n) && (s[i] == " "))) {
		i = (i + 1);
	}
	dynamic sign = 1;
	if (((i < n) && (((s[i] == "+") || (s[i] == "-"))))) {
		if ((s[i] == "-")) {
			sign = -1;
		}
		i = (i + 1);
	}
	dynamic digits = {"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9};
	dynamic result = 0;
	while ((i < n)) {
		dynamic ch = s[i];
		if (!((digits.contains(ch)))) {
			break;
		}
		dynamic d = digits[ch];
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
