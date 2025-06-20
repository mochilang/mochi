dynamic mergeKLists(lists) {
	dynamic k = lists.length;
	dynamic indices = [];
	dynamic i = 0;
	while ((i < k)) {
		indices = (indices + [0]);
		i = ((i + 1)).toInt();
	}
	dynamic result = [];
	while (true) {
		dynamic best = 0;
		dynamic bestList = -1;
		dynamic found = false;
		dynamic j = 0;
		while ((j < k)) {
			dynamic idx = indices[j];
			if ((idx < lists[j].length)) {
				dynamic val = lists[j][idx];
				if ((!found || (val < best))) {
					best = val;
					bestList = j;
					found = true;
				}
			}
			j = ((j + 1)).toInt();
		}
		if (!found) {
			break;
		}
		result = (result + [best]);
		indices[bestList] = (indices[bestList] + 1);
	}
	return result;
}

void main() {
}

