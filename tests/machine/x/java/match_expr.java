import java.util.*;
import java.util.function.*;
public class Main {
	static int x = 2;
	static Object label = (new java.util.function.Supplier<String>(){public String get(){
	var _t1 = x;
	if (Objects.equals(_t1, 1)) return "one";
	else if (Objects.equals(_t1, 2)) return "two";
	else if (Objects.equals(_t1, 3)) return "three";
	return "unknown";
}}).get();
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(label);
	}
}
