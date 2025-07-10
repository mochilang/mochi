import java.util.*;
public class Main {
	static Map<String,Integer> scores = Main.<String,Integer>mapOf("alice", 1);
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	scores.put("bob", 2);
	System.out.println(scores.get("bob"));
	}
}
