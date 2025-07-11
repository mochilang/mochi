import java.util.*;
public class Main {
	static List<Integer> nums = new ArrayList<>(Arrays.asList(1, 2));
	public static void main(String[] args) {
	nums.set(1, 3);
	System.out.println(nums.get(1));
	}
}
