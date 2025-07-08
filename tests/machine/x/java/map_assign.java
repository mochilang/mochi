import java.util.*;
public class Main {
	static Map<String,Integer> scores = new HashMap<>(java.util.Map.of("alice", 1));
	public static void main(String[] args) {
	scores.put("bob", 2);
	System.out.println(scores.get("bob"));
	}
}
