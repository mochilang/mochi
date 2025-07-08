import java.util.*;
public class Main {
	static int nums = java.util.Arrays.asList(3, 1, 4);
	static int min(List<Integer> v) {
		return Collections.min(v);
	}
	static int max(List<Integer> v) {
		return Collections.max(v);
	}
	public static void main(String[] args) {
	System.out.println(min(nums));
	System.out.println(max(nums));
	}
}
