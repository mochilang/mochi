import java.util.*;
public class Main {
	static Map<String,Integer> m = new HashMap<>(java.util.Map.of("a", 1, "b", 2));
	public static void main(String[] args) {
	for (var k : m) {
		System.out.println(k);
	}
	}
}
