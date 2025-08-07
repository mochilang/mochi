public class Main {
    static int[] example1;
    static int[] example2;
    static int[] example3;

    static int fibonacci(int k) {
        if (k < 0) {
            throw new RuntimeException(String.valueOf("k must be >= 0"));
        }
        int a = 0;
        int b = 1;
        int i = 0;
        while (i < k) {
            int tmp = a + b;
            a = b;
            b = tmp;
            i = i + 1;
        }
        return a;
    }

    static int min_int(int a, int b) {
        if (a < b) {
            return a;
        } else {
            return b;
        }
    }

    static int fibonacci_search(int[] arr, int val) {
        int n = arr.length;
        int m = 0;
        while (fibonacci(m) < n) {
            m = m + 1;
        }
        int offset = 0;
        while (m > 0) {
            int i_1 = min_int(offset + fibonacci(m - 1), n - 1);
            int item = arr[i_1];
            if (item == val) {
                return i_1;
            } else             if (val < item) {
                m = m - 1;
            } else {
                offset = offset + fibonacci(m - 1);
                m = m - 2;
            }
        }
        return -1;
    }
    public static void main(String[] args) {
        example1 = ((int[])(new int[]{4, 5, 6, 7}));
        example2 = ((int[])(new int[]{-18, 2}));
        example3 = ((int[])(new int[]{0, 5, 10, 15, 20, 25, 30}));
        System.out.println(_p(fibonacci_search(((int[])(example1)), 4)));
        System.out.println(_p(fibonacci_search(((int[])(example1)), -10)));
        System.out.println(_p(fibonacci_search(((int[])(example2)), -18)));
        System.out.println(_p(fibonacci_search(((int[])(example3)), 15)));
        System.out.println(_p(fibonacci_search(((int[])(example3)), 17)));
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
