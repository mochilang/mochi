import java.util.*;
public class Main {
	static int m = java.util.Map.of("a", 1, "b", 2, "c", 3);
	static <K,V> List<V> values(Map<K,V> m) {
		return new ArrayList<>(m.values());
	}
	public static void main(String[] args) {
	System.out.println(values(m));
	}
}
