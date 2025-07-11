import java.util.*;
public class InOperator {
	static List<Integer> xs = new ArrayList<>(Arrays.asList(1, 2, 3));
	static boolean inOp(Object item, Object collection) {
		if (collection instanceof Map<?,?> m) return m.containsKey(item);
		if (collection instanceof Collection<?> c) return c.contains(item);
		if (collection instanceof String s) return s.contains(String.valueOf(item));
		return false;
	}
	public static void main(String[] args) {
	System.out.println(inOp(2, xs));
	System.out.println(!(inOp(5, xs)));
	}
}
