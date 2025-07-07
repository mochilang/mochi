public class Main {
	static int sum(java.util.List<Integer> nums) {
		int s = 0;
		for (int n : nums) s += n;
		return s;
	}
	public static void main(String[] args) {
		System.out.println(sum(new java.util.ArrayList<>(java.util.Arrays.asList(1,2,3))));
	}
}
