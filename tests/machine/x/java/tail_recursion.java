import java.util.*;
public class Main {
	static int sum_rec(int n, int acc) {
		if (n == 0) {
			return acc;
		}
		return sum_rec(n - 1, acc + n);
	}
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(sum_rec(10, 0));
	}
}
