dynamic threeSum(nums) {
	dynamic sorted = (() {
	var _res = [];
	for (var x in nums) {
		_res.add(x);
	}
	var items = List.from(_res);
	items.sort((xA, xB) {
		var x = xA;
		var keyA = x;
		x = xB;
		var keyB = x;
		return Comparable.compare(keyA, keyB);
	});
	_res = items;
	return _res;
})();
	dynamic n = sorted.length;
	dynamic res = [];
	dynamic i = 0;
	while ((i < n)) {
		if (((i > 0) && (sorted[i] == sorted[(i - 1)]))) {
			i = ((i + 1)).toInt();
			continue;
		}
		dynamic left = (i + 1);
		dynamic right = (n - 1);
		while ((left < right)) {
			dynamic sum = ((sorted[i] + sorted[left]) + sorted[right]);
			if ((sum == 0)) {
				res = (res + [[sorted[i], sorted[left], sorted[right]]]);
				left = ((left + 1)).toInt();
				while (((left < right) && (sorted[left] == sorted[(left - 1)]))) {
					left = ((left + 1)).toInt();
				}
				right = ((right - 1)).toInt();
				while (((left < right) && (sorted[right] == sorted[(right + 1)]))) {
					right = ((right - 1)).toInt();
				}
			} else 
			if ((sum < 0)) {
				left = ((left + 1)).toInt();
			} else {
				right = ((right - 1)).toInt();
			}
		}
		i = ((i + 1)).toInt();
	}
	return res;
}

void main() {
}

