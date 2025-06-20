int removeElement(nums, int val) {
	dynamic k = 0;
	dynamic i = 0;
	while ((i < nums.length)) {
		if ((nums[i] != val)) {
			nums[k] = nums[i];
			k = ((k + 1)).toInt();
		}
		i = ((i + 1)).toInt();
	}
	return k;
}

void main() {
}

