public class Main {

    static void bar(int a, int b, int c) {
        System.out.println(String.valueOf(a) + ", " + String.valueOf(b) + ", " + String.valueOf(c));
    }

    static void main() {
        java.util.Map<String,Integer> args = new java.util.LinkedHashMap<String, Integer>();
args.put("a", 3);
args.put("b", 2);
args.put("c", 1);
        bar((int)(((int)args.getOrDefault("a", 0))), (int)(((int)args.getOrDefault("b", 0))), (int)(((int)args.getOrDefault("c", 0))));
    }
    public static void main(String[] args) {
        main();
    }
}
