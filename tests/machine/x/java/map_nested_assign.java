import java.util.*;
public class Main {
	static Map<String,Map<String,Integer>> data = Main.<String,Map<String,Integer>>mapOf("outer", Main.<String,Integer>mapOf("inner", 1));
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	((Map)data.get("outer")).put("inner", 2);
	System.out.println(((Map)data.get("outer")).get("inner"));
	}
}
