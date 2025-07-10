import java.util.*;
public class Main {
	static Map<String,Integer> m = new LinkedHashMap<>(){{put("a", 1);put("b", 2);put("c", 3);}};
	static <K,V> List<V> values(Map<K,V> m) {
		return new ArrayList<>(m.values());
	}
	public static void main(String[] args) {
	System.out.println(values(m));
	}
}
