dynamic findSubstring(String s, words) {
	if ((words.length == 0)) {
		return [];
	}
	dynamic wordLen = words[0].length;
	dynamic wordCount = words.length;
	dynamic totalLen = (wordLen * wordCount);
	if ((s.length < totalLen)) {
		return [];
	}
	dynamic freq = {};
	for (var w in words) {
		if ((freq.containsKey(w))) {
			freq[w] = (freq[w] + 1);
		} else {
			freq[w] = 1;
		}
	}
	dynamic result = [];
	for (var offset = 0; offset < wordLen; offset++) {
		dynamic left = offset;
		dynamic count = 0;
		dynamic seen = {};
		dynamic j = offset;
		while (((j + wordLen) <= s.length)) {
			dynamic word = s.substring(j, (j + wordLen));
			j = (j + wordLen);
			if ((freq.containsKey(word))) {
				if ((seen.containsKey(word))) {
					seen[word] = (seen[word] + 1);
				} else {
					seen[word] = 1;
				}
				count = (count + 1);
				while ((seen[word] > freq[word])) {
					dynamic lw = s.substring(left, (left + wordLen));
					seen[lw] = (seen[lw] - 1);
					left = (left + wordLen);
					count = (count - 1);
				}
				if ((count == wordCount)) {
					result = (result + [left]);
					dynamic lw = s.substring(left, (left + wordLen));
					seen[lw] = (seen[lw] - 1);
					left = (left + wordLen);
					count = (count - 1);
				}
			} else {
				seen = {};
				count = 0;
				left = j;
			}
		}
	}
	return result;
}

void main() {
}

