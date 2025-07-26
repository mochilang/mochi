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
        Object[] ab = f();
        Object a = ab[0];
        Object b = ab[1];
        Object cb = f()[1];
        int d = g(((Number)(a)).intValue(), ((Number)(cb)).doubleValue());
        int e = g(d, ((Number)(b)).doubleValue());
        int i = g(d, 2.0);
        int[] list = new int[]{};
        list = java.util.stream.IntStream.concat(java.util.Arrays.stream(list), java.util.stream.IntStream.of(((Number)(a)).intValue())).toArray();
        list = java.util.stream.IntStream.concat(java.util.Arrays.stream(list), java.util.stream.IntStream.of(d)).toArray();
        list = java.util.stream.IntStream.concat(java.util.Arrays.stream(list), java.util.stream.IntStream.of(e)).toArray();
        list = java.util.stream.IntStream.concat(java.util.Arrays.stream(list), java.util.stream.IntStream.of(i)).toArray();
        i = list.length;
    }
    public static void main(String[] args) {
        main();
    }
}
