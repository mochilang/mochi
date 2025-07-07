import java.util.*;

public class JsonBuiltin {
    public static void main(String[] args) {
        Map<String, Integer> m = new LinkedHashMap<>();
        m.put("a", 1);
        m.put("b", 2);
        String json = "{" + "\"a\":" + m.get("a") + ",\"b\":" + m.get("b") + "}";
        System.out.println(json);
    }
}
