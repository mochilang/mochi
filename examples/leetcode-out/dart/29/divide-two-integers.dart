int divide(int dividend, int divisor) {
	if (((dividend == ((-2147483647 - 1))) && (divisor == (-1)))) {
		return 2147483647;
	}
	dynamic negative = false;
	if ((dividend < 0)) {
		negative = !negative;
		dividend = (-dividend).toInt();
	}
	if ((divisor < 0)) {
		negative = !negative;
		divisor = (-divisor).toInt();
	}
	dynamic quotient = 0;
	while ((dividend >= divisor)) {
		dynamic temp = divisor;
		dynamic multiple = 1;
		while ((dividend >= (temp + temp))) {
			temp = ((temp + temp)).toInt();
			multiple = ((multiple + multiple)).toInt();
		}
		dividend = ((dividend - temp)).toInt();
		quotient = ((quotient + multiple)).toInt();
	}
	if (negative) {
		quotient = (-quotient).toInt();
	}
	if ((quotient > 2147483647)) {
		return 2147483647;
	}
	if ((quotient < ((-2147483647 - 1)))) {
		return -2147483648;
	}
	return quotient;
}

void main() {
}

