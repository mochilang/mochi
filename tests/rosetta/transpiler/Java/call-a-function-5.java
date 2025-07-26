public class Main {

    static int doIt(java.util.Map<String,Integer> p) {
        int b = 0;
        if (p.containsKey("b")) {
            b = (int)(((int)p.getOrDefault("b", 0)));
        }
        return (int)(((int)p.getOrDefault("a", 0))) + b + (int)(((int)p.getOrDefault("c", 0)));
    }

    static void main() {
        java.util.Map<String,Integer> p = new java.util.LinkedHashMap<String, Integer>();
p.put("a", 1);
p.put("c", 9);
        System.out.println(String.valueOf(doIt(p)));
    }
    public static void main(String[] args) {
        main();
    }
}
