import java.util.*;
public class Main {
	static Map<Integer,String> m = Main.<Integer,String>mapOf(1, "a", 2, "b");
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(m.get(1));
	}
}
