import java.util.*;
public class Main {
	static int result = twoSum(java.util.Arrays.asList(2, 7, 11, 15), 9);
	static int twoSum(int nums, int target) {
		int n = nums.size();
		for (int i = 0; i < n; i++) {
			for (int j = i + 1; j < n; j++) {
				if (nums.get(i) + nums.get(j) == target) {
					return java.util.Arrays.asList(i, j);
				}
			}
		}
		return java.util.Arrays.asList(-1, -1);
	}
	public static void main(String[] args) {
	System.out.println(result.get(0));
	System.out.println(result.get(1));
	}
}
