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
        h("ex1", new int[]{});
        h("ex2", new int[]{1, 2});
        h("ex3", new int[]{1, 2, 3, 4});
        int[] list = new int[]{1, 2, 3, 4};
        h("ex4", list);
    }
    public static void main(String[] args) {
        main();
    }
}
