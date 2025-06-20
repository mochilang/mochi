public class Main {
	static int removeElement(int[] nums, int val) {
		int k = 0;
		int i = 0;
		while ((i < nums.length)) {
			if ((nums[i] != val)) {
				nums[k] = nums[i];
				k = (k + 1);
			}
			i = (i + 1);
		}
		return k;
	}
	
	public static void main(String[] args) {
	}
}
