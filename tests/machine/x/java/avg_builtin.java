import java.util.*;
public class Main {
	static double avg(List<? extends Number> v) {
		if (v.isEmpty()) return 0;
		int s = 0;
		for (Number n : v) s += n.intValue();
		return (double)s / v.size();
	}
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(avg((List<Number>)(List<?>)java.util.Arrays.asList(1, 2, 3)));
	}
}
