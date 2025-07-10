import java.util.*;
public class Main {
	static List<Integer> xs = new ArrayList<>(java.util.Arrays.asList(1, 2, 3));
	static List<Object> ys = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var x : xs) {
		if (!(Objects.equals(x % 2, 1))) continue;
		_res1.add(x);
	}
	return _res1;
}}).get();
	static Map<String,Integer> m = new LinkedHashMap<String,Integer>(){{put("a", 1);}};
	static String s = "hello";
	static boolean inOp(Object item, Object collection) {
		if (collection instanceof Map<?,?> m) return m.containsKey(item);
		if (collection instanceof Collection<?> c) return c.contains(item);
		if (collection instanceof String s) return s.contains(String.valueOf(item));
		return false;
	}
	public static void main(String[] args) {
	System.out.println(inOp(1, ys));
	System.out.println(inOp(2, ys));
	System.out.println(inOp("a", m));
	System.out.println(inOp("b", m));
	System.out.println(inOp("ell", s));
	System.out.println(inOp("foo", s));
	}
}
