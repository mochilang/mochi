dynamic lengthOfLongestSubstring(s) {
	dynamic n = s.length;
	dynamic start = 0;
	dynamic best = 0;
	dynamic i = 0;
	while ((i < n)) {
		dynamic j = start;
		while ((j < i)) {
			if ((s[j] == s[i])) {
				start = (j + 1);
				break;
			}
			j = (j + 1);
		}
		dynamic length = ((i - start) + 1);
		if ((length > best)) {
			best = length;
		}
		i = (i + 1);
	}
	return best;
}

void main() {
}
