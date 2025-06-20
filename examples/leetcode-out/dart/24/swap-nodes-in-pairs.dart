dynamic swapPairs(nums) {
	dynamic i = 0;
	dynamic result = [];
	while ((i < nums.length)) {
		if (((i + 1) < nums.length)) {
			result = (result + [nums[(i + 1)], nums[i]]);
		} else {
			result = (result + [nums[i]]);
		}
		i = ((i + 2)).toInt();
	}
	return result;
}

void main() {
}

