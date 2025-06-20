dynamic removeNthFromEnd(nums, int n) {
	dynamic idx = (nums.length - n);
	dynamic result = [];
	dynamic i = 0;
	while ((i < nums.length)) {
		if ((i != idx)) {
			result = (result + [nums[i]]);
		}
		i = ((i + 1)).toInt();
	}
	return result;
}

void main() {
}

