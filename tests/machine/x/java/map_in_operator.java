import java.util.*;
public class Main {
	static Map<Integer,String> m = new LinkedHashMap<Integer,String>(){{put(1, "a");put(2, "b");}};
	static boolean inOp(Object item, Object collection) {
		if (collection instanceof Map<?,?> m) return m.containsKey(item);
		if (collection instanceof Collection<?> c) return c.contains(item);
		if (collection instanceof String s) return s.contains(String.valueOf(item));
		return false;
	}
	public static void main(String[] args) {
	System.out.println(inOp(1, m));
	System.out.println(inOp(3, m));
	}
}
