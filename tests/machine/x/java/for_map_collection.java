import java.util.*;
public class Main {
	static Map<Object,Object> m = map("a", 1, "b", 2);
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	for (var k : m.keySet()) {
		System.out.println(k);
	}
	}
}
