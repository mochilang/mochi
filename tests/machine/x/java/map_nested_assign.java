import java.util.*;
public class Main {
	static Map<String,Object> data = new HashMap<>(java.util.Map.of("outer", java.util.Map.of("inner", 1)));
	public static void main(String[] args) {
	data.get("outer").put("inner", 2);
	System.out.println(data.get("outer").get("inner"));
	}
}
