import java.util.*;
public class MapAssign {
    public static void main(String[] args) {
        Map<String, Integer> scores = new HashMap<>();
        scores.put("alice", 1);
        scores.put("bob", 2);
        System.out.println(scores.get("bob"));
    }
}
