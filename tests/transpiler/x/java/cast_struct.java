public class Main {
    static java.util.Map todo = new java.util.LinkedHashMap<>() {{ put("title", "hi"); }};

    public static void main(String[] args) {
        System.out.println(todo.title);
    }
}
