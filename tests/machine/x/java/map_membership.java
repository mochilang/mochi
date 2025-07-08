import java.util.*;
public class Main {
	static Map<String,Integer> m = new HashMap<>(java.util.Map.of("a", 1, "b", 2));
	static boolean inOp(Object item, Object collection) {
		if (collection instanceof Map<?,?> m) return m.containsKey(item);
		if (collection instanceof Collection<?> c) return c.contains(item);
		if (collection instanceof String s) return s.contains(String.valueOf(item));
		return false;
	}
	public static void main(String[] args) {
	System.out.println(inOp("a", m));
	System.out.println(inOp("c", m));
	}
}
