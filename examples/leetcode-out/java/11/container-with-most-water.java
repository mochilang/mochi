public class Main {
	static int maxArea(int[] height) {
		int left = 0;
		int right = (height.length - 1);
		int maxArea = 0;
		while ((left < right)) {
			int width = (right - left);
			int h = 0;
			if ((height[left] < height[right])) {
				h = height[left];
			} else {
				h = height[right];
			}
			int area = (h * width);
			if ((area > maxArea)) {
				maxArea = area;
			}
			if ((height[left] < height[right])) {
				left = (left + 1);
			} else {
				right = (right - 1);
			}
		}
		return maxArea;
	}
	
	public static void main(String[] args) {
	}
}
