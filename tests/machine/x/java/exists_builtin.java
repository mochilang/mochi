import java.util.*;
public class Main {
	static List<Integer> data = new ArrayList<>(java.util.Arrays.asList(1, 2));
	static Object flag = data.stream().anyMatch(x -> Objects.equals(x, 1));
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(flag);
	}
}
