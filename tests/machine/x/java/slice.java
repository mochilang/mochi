import java.util.*;
public class Main {
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(((List)java.util.Arrays.asList(1, 2, 3)).subList(1, 3));
	System.out.println(((List)java.util.Arrays.asList(1, 2, 3)).subList(0, 2));
	System.out.println("hello".substring(1, 4));
	}
}
