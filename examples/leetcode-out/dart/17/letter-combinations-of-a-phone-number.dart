dynamic letterCombinations(String digits) {
	if ((digits.length == 0)) {
		return [];
	}
	dynamic mapping = {"2": ["a", "b", "c"], "3": ["d", "e", "f"], "4": ["g", "h", "i"], "5": ["j", "k", "l"], "6": ["m", "n", "o"], "7": ["p", "q", "r", "s"], "8": ["t", "u", "v"], "9": ["w", "x", "y", "z"]};
	dynamic result = [""];
	var _tmp0 = digits;
	for (var _tmp1 in _tmp0.runes) {
		var d = String.fromCharCode(_tmp1);
		if (!((mapping.containsKey(d)))) {
			continue;
		}
		dynamic letters = mapping[d];
		dynamic next = (() {
	var _res = [];
	for (var p in result) {
		for (var ch in letters) {
			_res.add((p + ch));
		}
	}
	return _res;
})();
		result = next;
	}
	return result;
}

void main() {
}

