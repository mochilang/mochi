int removeDuplicates(nums) {
	if ((nums.length == 0)) {
		return 0;
	}
	dynamic count = 1;
	dynamic prev = nums[0];
	dynamic i = 1;
	while ((i < nums.length)) {
		dynamic cur = nums[i];
		if ((cur != prev)) {
			count = (count + 1);
			prev = cur;
		}
		i = ((i + 1)).toInt();
	}
	return count;
}

void main() {
}

