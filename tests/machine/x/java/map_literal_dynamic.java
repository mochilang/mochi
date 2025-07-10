import java.util.*;
public class Main {
	static int x = 3;
	static int y = 4;
	static Map<Object,Object> m = map("a", x, "b", y);
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(m.get("a") + " " + m.get("b"));
	}
}
