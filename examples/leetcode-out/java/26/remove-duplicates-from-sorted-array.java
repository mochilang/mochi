public class Main {
	static int removeDuplicates(int[] nums) {
		if ((nums.length == 0)) {
			return 0;
		}
		int count = 1;
		int prev = nums[0];
		int i = 1;
		while ((i < nums.length)) {
			int cur = nums[i];
			if ((cur != prev)) {
				count = (count + 1);
				prev = cur;
			}
			i = (i + 1);
		}
		return count;
	}
	
	public static void main(String[] args) {
	}
}
