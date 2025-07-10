import java.util.*;
public class Main {
	static int count(Collection<?> c) {
		return c.size();
	}
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(count(java.util.Arrays.asList(1, 2, 3)));
	}
}
