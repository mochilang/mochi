int reverse(int x) {
	dynamic sign = 1;
	dynamic n = x;
	if ((n < 0)) {
		sign = (-1).toInt();
		n = (-n).toInt();
	}
	dynamic rev = 0;
	while ((n != 0)) {
		dynamic digit = (n % 10);
		rev = (((rev * 10) + digit)).toInt();
		n = ((n ~/ 10)).toInt();
	}
	rev = ((rev * sign)).toInt();
	if (((rev < ((-2147483647 - 1))) || (rev > 2147483647))) {
		return 0;
	}
	return rev;
}

void main() {
}

