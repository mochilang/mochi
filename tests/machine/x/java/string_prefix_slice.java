import java.util.*;
public class Main {
	static String prefix = "fore";
	static String s1 = "forest";
	static String s2 = "desert";
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(Objects.equals(s1.substring(0, prefix.length()), prefix));
	System.out.println(Objects.equals(s2.substring(0, prefix.length()), prefix));
	}
}
