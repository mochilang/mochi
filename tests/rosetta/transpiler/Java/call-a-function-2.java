public class Main {

    static Object[] f() {
        return new Object[]{0, 0.0};
    }

    static int g(int a, double b) {
        return 0;
    }

    static void h(String s, int[] nums) {
    }

    static void main() {
        f();
        g(1, 2.0);
        Object[] res = f();
        g(((Number)(res[0])).intValue(), ((Number)(res[1])).doubleValue());
        g(g(1, 2.0), 3.0);
    }
    public static void main(String[] args) {
        main();
    }
}
