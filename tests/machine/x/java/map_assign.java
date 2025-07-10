import java.util.*;
public class Main {
	static Map<String,Integer> scores = new LinkedHashMap<String,Integer>(){{put("alice", 1);}};
	public static void main(String[] args) {
	scores.put("bob", 2);
	System.out.println(scores.get("bob"));
	}
}
