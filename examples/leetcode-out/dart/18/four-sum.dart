dynamic fourSum(nums, int target) {
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
	dynamic result = [];
	for (var i = 0; i < n; i++) {
		if (((i > 0) && (sorted[i] == sorted[(i - 1)]))) {
			continue;
		}
		for (var j = (i + 1); j < n; j++) {
			if (((j > (i + 1)) && (sorted[j] == sorted[(j - 1)]))) {
				continue;
			}
			dynamic left = (j + 1);
			dynamic right = (n - 1);
			while ((left < right)) {
				dynamic sum = (((sorted[i] + sorted[j]) + sorted[left]) + sorted[right]);
				if ((sum == target)) {
					result = (result + [[sorted[i], sorted[j], sorted[left], sorted[right]]]);
					left = (left + 1);
					right = ((right - 1)).toInt();
					while (((left < right) && (sorted[left] == sorted[(left - 1)]))) {
						left = (left + 1);
					}
					while (((left < right) && (sorted[right] == sorted[(right + 1)]))) {
						right = ((right - 1)).toInt();
					}
				} else 
				if ((sum < target)) {
					left = (left + 1);
				} else {
					right = ((right - 1)).toInt();
				}
			}
		}
	}
	return result;
}

void main() {
}

