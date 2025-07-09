import java.util.*;
public class Main {
	static Map<String,Object> data = new HashMap<>(new HashMap<>(java.util.Map.of("outer", new HashMap<>(java.util.Map.of("inner", 1)))));
	public static void main(String[] args) {
	((Map)((Map)data).get("outer")).put("inner", 2);
	System.out.println(((Map)((Map)data).get("outer")).get("inner"));
	}
}
