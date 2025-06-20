#include <stdio.h>
#include <stdlib.h>

typedef struct { int len; int *data; } list_int;

static list_int list_int_create(int len) {
	list_int l;
	l.len = len;
	l.data = (int*)malloc(sizeof(int)*len);
	return l;
}


int removeDuplicates(list_int nums){
	if ((nums.len == 0)) {
		return 0;
	}
	int count = 1;
	int prev = nums.data[0];
	int i = 1;
	while ((i < nums.len)) {
		int cur = nums.data[i];
		if ((cur != prev)) {
			count = (count + 1);
			prev = cur;
		}
		i = (i + 1);
	}
	return count;
}

int main() {
	return 0;
}
