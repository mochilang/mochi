import java.util.*;
public class Main {
	static Map<String,Integer> m = new LinkedHashMap<String,Integer>(){{put("a", 1);put("b", 2);}};
	public static void main(String[] args) {
	for (var k : m.keySet()) {
		System.out.println(k);
	}
	}
}
