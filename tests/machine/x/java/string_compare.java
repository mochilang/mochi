import java.util.*;
public class Main {
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println("a".compareTo("b") < 0);
	System.out.println("a".compareTo("a") <= 0);
	System.out.println("b".compareTo("a") > 0);
	System.out.println("b".compareTo("b") >= 0);
	}
}
