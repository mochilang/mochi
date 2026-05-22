public class Main {
    static Counter c = new Counter(0);

    static void inc(Counter c) {
c.put("n", ((Integer) (c.get("n"))) + 1);
    }
    public static void main(String[] args) {
        inc(c);
        System.out.println(((Integer) (c.get("n"))));
    }
}
