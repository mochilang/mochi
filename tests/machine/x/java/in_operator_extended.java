import java.util.*;
class MA {
	int a;
	MA(int a) {
		this.a = a;
	}
}
public class InOperatorExtended {
	static List<Integer> xs = new ArrayList<>(Arrays.asList(1, 2, 3));
	static List<Integer> ys = (new java.util.function.Supplier<List<Integer>>(){public List<Integer> get(){
	List<Integer> _res1 = new ArrayList<>();
	for (var x : xs) {
		if (!(Objects.equals(x % 2, 1))) continue;
		_res1.add(x);
	}
	return _res1;
}}).get();
	static MA m = new MA(1);
	static String s = "hello";
	static boolean inOp(Object item, Object collection) {
		if (collection instanceof Map<?,?> m) return m.containsKey(item);
		if (collection instanceof Collection<?> c) return c.contains(item);
		if (collection instanceof String s) return s.contains(String.valueOf(item));
		return false;
	}
	public static void main(String[] args) {
	System.out.println(ys.contains(1));
	System.out.println(ys.contains(2));
	System.out.println(inOp("a", m));
	System.out.println(inOp("b", m));
	System.out.println(s.contains(String.valueOf("ell")));
	System.out.println(s.contains(String.valueOf("foo")));
	}
}
