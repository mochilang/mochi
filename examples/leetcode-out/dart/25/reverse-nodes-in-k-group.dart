dynamic reverseKGroup(nums, int k) {
	dynamic n = nums.length;
	if ((k <= 1)) {
		return nums;
	}
	dynamic result = [];
	dynamic i = 0;
	while ((i < n)) {
		dynamic end = (i + k);
		if ((end <= n)) {
			dynamic j = (end - 1);
			while ((j >= i)) {
				result = (result + [nums[j]]);
				j = ((j - 1)).toInt();
			}
		} else {
			dynamic j = i;
			while ((j < n)) {
				result = (result + [nums[j]]);
				j = ((j + 1)).toInt();
			}
		}
		i = ((i + k)).toInt();
	}
	return result;
}

void main() {
}

