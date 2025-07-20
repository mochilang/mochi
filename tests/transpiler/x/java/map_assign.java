public class Main {
    static java.util.Map scores = new java.util.LinkedHashMap<>() {{ put("alice", 1); }};

    public static void main(String[] args) {
scores.put("bob", 2);
        System.out.println(scores.get("bob"));
    }
}
