import java.util.*;
public class Main {
	static <T> List<T> union_all(List<T> a, List<T> b) {
		List<T> res = new ArrayList<>(a);
		res.addAll(b);
		return res;
	}
	static <T> List<T> union(List<T> a, List<T> b) {
		LinkedHashSet<T> s = new LinkedHashSet<>(a);
		s.addAll(b);
		return new ArrayList<>(s);
	}
	static <T> List<T> except(List<T> a, List<T> b) {
		List<T> res = new ArrayList<>();
		for (T x : a) if (!b.contains(x)) res.add(x);
		return res;
	}
	static <T> List<T> intersect(List<T> a, List<T> b) {
		List<T> res = new ArrayList<>();
		for (T x : a) if (b.contains(x) && !res.contains(x)) res.add(x);
		return res;
	}
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(union(java.util.Arrays.asList(1, 2), java.util.Arrays.asList(2, 3)));
	System.out.println(except(java.util.Arrays.asList(1, 2, 3), java.util.Arrays.asList(2)));
	System.out.println(intersect(java.util.Arrays.asList(1, 2, 3), java.util.Arrays.asList(2, 4)));
	System.out.println(union_all(java.util.Arrays.asList(1, 2), java.util.Arrays.asList(2, 3)).size());
	}
}
