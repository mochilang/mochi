import java.util.*;
public class Main {
	static boolean boom() {
		System.out.println("boom");
		return true;
	}
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println((1 < 2) && (2 < 3) && (3 < 4));
	System.out.println((1 < 2) && (2 > 3) && boom());
	System.out.println((1 < 2) && (2 < 3) && (3 > 4) && boom());
	}
}
