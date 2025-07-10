import java.util.*;
public class Main {
	static Map<Integer,String> m = new LinkedHashMap<>(){{put(1, "a");put(2, "b");}};
	public static void main(String[] args) {
	System.out.println(m.get(1));
	}
}
