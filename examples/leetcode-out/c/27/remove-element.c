#include <stdio.h>
#include <stdlib.h>

typedef struct { int len; int *data; } list_int;

static list_int list_int_create(int len) {
	list_int l;
	l.len = len;
	l.data = (int*)malloc(sizeof(int)*len);
	return l;
}


int removeElement(list_int nums, int val){
	int k = 0;
	int i = 0;
	while ((i < nums.len)) {
		if ((nums.data[i] != val)) {
			nums.data[k] = nums.data[i];
			k = (k + 1);
		}
		i = (i + 1);
	}
	return k;
}

int main() {
	return 0;
}
