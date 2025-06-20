String intToRoman(int num) {
	dynamic values = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
	dynamic symbols = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"];
	dynamic result = "";
	dynamic i = 0;
	while ((num > 0)) {
		while ((num >= values[i])) {
			result = (result + symbols[i]);
			num = ((num - values[i])).toInt();
		}
		i = ((i + 1)).toInt();
	}
	return result;
}

void main() {
}

