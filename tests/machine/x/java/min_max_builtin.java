import java.util.*;
public class MinMaxBuiltin {
	static List<Integer> nums = new ArrayList<>(Arrays.asList(3, 1, 4));
	public static void main(String[] args) {
	System.out.println(nums.stream().mapToInt(n -> ((Number)n).intValue()).min().orElse(Integer.MAX_VALUE));
	System.out.println(nums.stream().mapToInt(n -> ((Number)n).intValue()).max().orElse(Integer.MIN_VALUE));
	}
}
