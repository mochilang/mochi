public class Main {
    static int[] memo = new int[0];
    static int[] results = new int[0];

    static int factorial(int num) {
        if (num < 0) {
            System.out.println("Number should not be negative.");
            return 0;
        }
        int[] m = ((int[])(memo));
        int i = m.length;
        while (i <= num) {
            m = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(m), java.util.stream.IntStream.of(i * m[i - 1])).toArray()));
            i = i + 1;
        }
        memo = ((int[])(m));
        return m[num];
    }
    public static void main(String[] args) {
        memo = ((int[])(new int[]{1, 1}));
        System.out.println(_p(factorial(7)));
        factorial(-1);
        results = ((int[])(new int[]{}));
        for (int i = 0; i < 10; i++) {
            results = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(results), java.util.stream.IntStream.of(factorial(i))).toArray()));
        }
        System.out.println(_p(results));
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
