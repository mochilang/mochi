dynamic reverse(x) {
	dynamic sign = 1;
	dynamic n = x;
	if ((n < 0)) {
		sign = -1;
		n = -n;
	}
	dynamic rev = 0;
	while ((n != 0)) {
		dynamic digit = (n % 10);
		rev = ((rev * 10) + digit);
		n = (n ~/ 10);
	}
	rev = (rev * sign);
	if (((rev < ((-2147483647 - 1))) || (rev > 2147483647))) {
		return 0;
	}
	return rev;
}

void main() {
}
