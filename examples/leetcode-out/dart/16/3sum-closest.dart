int threeSumClosest(nums, int target) {
	dynamic sorted = (() {
	var _res = [];
	for (var n in nums) {
		_res.add(n);
	}
	var items = List.from(_res);
	items.sort((nA, nB) {
		var n = nA;
		var keyA = n;
		n = nB;
		var keyB = n;
		return Comparable.compare(keyA, keyB);
	});
	_res = items;
	return _res;
})();
	dynamic n = sorted.length;
	dynamic best = ((sorted[0] + sorted[1]) + sorted[2]);
	for (var i = 0; i < n; i++) {
		dynamic left = (i + 1);
		dynamic right = (n - 1);
		while ((left < right)) {
			dynamic sum = ((sorted[i] + sorted[left]) + sorted[right]);
			if ((sum == target)) {
				return target;
			}
			dynamic diff = 0;
			if ((sum > target)) {
				diff = ((sum - target)).toInt();
			} else {
				diff = ((target - sum)).toInt();
			}
			dynamic bestDiff = 0;
			if ((best > target)) {
				bestDiff = ((best - target)).toInt();
			} else {
				bestDiff = ((target - best)).toInt();
			}
			if ((diff < bestDiff)) {
				best = sum;
			}
			if ((sum < target)) {
				left = (left + 1);
			} else {
				right = ((right - 1)).toInt();
			}
		}
	}
	return best;
}

void main() {
}

