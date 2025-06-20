#include <stdio.h>
#include <stdlib.h>

typedef struct { int len; int *data; } list_int;

static list_int list_int_create(int len) {
	list_int l;
	l.len = len;
	l.data = (int*)malloc(sizeof(int)*len);
	return l;
}


int maxArea(list_int height){
	int left = 0;
	int right = (height.len - 1);
	int maxArea = 0;
	while ((left < right)) {
		int width = (right - left);
		int h = 0;
		if ((height.data[left] < height.data[right])) {
			h = height.data[left];
		} else {
			h = height.data[right];
		}
		int area = (h * width);
		if ((area > maxArea)) {
			maxArea = area;
		}
		if ((height.data[left] < height.data[right])) {
			left = (left + 1);
		} else {
			right = (right - 1);
		}
	}
	return maxArea;
}

int main() {
	return 0;
}
