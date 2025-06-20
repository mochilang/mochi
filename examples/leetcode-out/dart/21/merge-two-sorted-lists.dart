dynamic mergeTwoLists(l1, l2) {
	dynamic i = 0;
	dynamic j = 0;
	dynamic result = [];
	while (((i < l1.length) && (j < l2.length))) {
		if ((l1[i] <= l2[j])) {
			result = (result + [l1[i]]);
			i = ((i + 1)).toInt();
		} else {
			result = (result + [l2[j]]);
			j = ((j + 1)).toInt();
		}
	}
	while ((i < l1.length)) {
		result = (result + [l1[i]]);
		i = ((i + 1)).toInt();
	}
	while ((j < l2.length)) {
		result = (result + [l2[j]]);
		j = ((j + 1)).toInt();
	}
	return result;
}

void main() {
}

