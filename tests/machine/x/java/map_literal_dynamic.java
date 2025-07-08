import java.util.*;
public class Main {
	static int x = 3;
	static int y = 4;
	static Map<String,Object> m = new HashMap<>(java.util.Map.of("a", x, "b", y));
	public static void main(String[] args) {
	System.out.println(m.get("a") + " " + m.get("b"));
	}
}
