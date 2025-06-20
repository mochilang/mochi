int maxArea(height) {
	dynamic left = 0;
	dynamic right = (height.length - 1);
	dynamic maxArea = 0;
	while ((left < right)) {
		dynamic width = (right - left);
		dynamic h = 0;
		if ((height[left] < height[right])) {
			h = (height[left]).toInt();
		} else {
			h = (height[right]).toInt();
		}
		dynamic area = (h * width);
		if ((area > maxArea)) {
			maxArea = area;
		}
		if ((height[left] < height[right])) {
			left = ((left + 1)).toInt();
		} else {
			right = ((right - 1)).toInt();
		}
	}
	return maxArea;
}

void main() {
}

