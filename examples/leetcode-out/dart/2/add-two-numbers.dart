List<int> addTwoNumbers(List<int> l1, List<int> l2) {
	int i = 0;
	int j = 0;
	int carry = 0;
	List<int> result = [];
	while ((((i < l1.length) || (j < l2.length)) || (carry > 0))) {
		int x = 0;
		if ((i < l1.length)) {
			x = l1[i];
			i = (i + 1);
		}
		int y = 0;
		if ((j < l2.length)) {
			y = l2[j];
			j = (j + 1);
		}
		var sum = ((x + y) + carry);
		var digit = (sum % 10);
		carry = (sum ~/ 10);
		result = (result + [digit]);
	}
	return result;
}

void main() {
}
