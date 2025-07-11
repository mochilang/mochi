import java.util.*;
public class Membership {
	static List<Integer> nums = new ArrayList<>(Arrays.asList(1, 2, 3));
	static boolean inOp(Object item, Object collection) {
		if (collection instanceof Map<?,?> m) return m.containsKey(item);
		if (collection instanceof Collection<?> c) return c.contains(item);
		if (collection instanceof String s) return s.contains(String.valueOf(item));
		return false;
	}
	public static void main(String[] args) {
	System.out.println(inOp(2, nums));
	System.out.println(inOp(4, nums));
	}
}
