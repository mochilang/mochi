public class Main {
	static int sum(java.util.List<Integer> nums) {
		int s = 0;
		for (int n : nums) s += n;
		return s;
	}
	static double avg(java.util.List<Integer> nums) {
		if (nums.isEmpty()) return 0;
		return (double)sum(nums) / nums.size();
	}
	public static void main(String[] args) {
		System.out.println(avg(new java.util.ArrayList<>(java.util.Arrays.asList(1,2,3))));
	}
}
