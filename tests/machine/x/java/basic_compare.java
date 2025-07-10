import java.util.*;
public class Main {
	static int a = 10 - 3;
	static int b = 2 + 2;
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(a);
	System.out.println(a == 7);
	System.out.println(b < 5);
	}
}
