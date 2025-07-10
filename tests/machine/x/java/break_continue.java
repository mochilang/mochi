import java.util.*;
public class Main {
	static List<Integer> numbers = new ArrayList<>(java.util.Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9));
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	for (var n : numbers) {
		if (Objects.equals(n % 2, 0)) {
			continue;
		}
		if (n > 7) {
			break;
		}
		System.out.println("odd number:" + " " + n);
	}
	}
}
