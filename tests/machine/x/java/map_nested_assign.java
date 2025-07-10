import java.util.*;
public class Main {
	static Map<String,Map<String,Integer>> data = new LinkedHashMap<>(){{put("outer", new LinkedHashMap<>(){{put("inner", 1);}});}};
	public static void main(String[] args) {
	((Map)data.get("outer")).put("inner", 2);
	System.out.println(((Map)data.get("outer")).get("inner"));
	}
}
