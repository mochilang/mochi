import java.util.*;
class AB {
	int a;
	int b;
	AB(int a, int b) {
		this.a = a;
		this.b = b;
	}
	int size() { return 2; }
}
public class MapMembership {
	static boolean inOp(Object item, Object collection) {
		if (collection instanceof Map<?,?> m) return m.containsKey(item);
		if (collection instanceof Collection<?> c) return c.contains(item);
		if (collection instanceof String s) return s.contains(String.valueOf(item));
		return false;
	}
	public static void main(String[] args) {
	AB m = new AB(1, 2);
	System.out.println(inOp("a", m));
	System.out.println(inOp("c", m));
	}
}
