public class Main {
    static java.util.Map data = new java.util.LinkedHashMap<>() {{ put("outer", new java.util.LinkedHashMap<>() {{ put("inner", 1); }}); }};

    public static void main(String[] args) {
data["outer"]["inner"] = 2;
        System.out.println(data.get("outer")["inner"]);
    }
}
