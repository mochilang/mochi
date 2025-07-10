import java.util.*;
public class Main {
	static List<List<Integer>> matrix = new ArrayList<>(java.util.Arrays.asList(java.util.Arrays.asList(1, 2), java.util.Arrays.asList(3, 4)));
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	((List)matrix.get(1)).set(0, 5);
	System.out.println(((List)matrix.get(1)).get(0));
	}
}
