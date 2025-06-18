dynamic addTwoNumbers(l1, l2) {
	dynamic i = 0;
	dynamic j = 0;
	dynamic carry = 0;
	dynamic result = [];
	while ((((i < l1.length) || (j < l2.length)) || (carry > 0))) {
		dynamic x = 0;
		if ((i < l1.length)) {
			x = l1[i];
			i = (i + 1);
		}
		dynamic y = 0;
		if ((j < l2.length)) {
			y = l2[j];
			j = (j + 1);
		}
		dynamic sum = ((x + y) + carry);
		dynamic digit = (sum % 10);
		carry = (sum ~/ 10);
		result = (result + [digit]);
	}
	return result;
}

void main() {
}
