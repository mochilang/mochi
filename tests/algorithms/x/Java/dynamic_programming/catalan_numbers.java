public class Main {

    static void panic(String msg) {
        System.out.println(msg);
    }

    static int[] catalan_numbers(int upper_limit) {
        if (upper_limit < 0) {
            panic("Limit for the Catalan sequence must be >= 0");
            return new int[]{};
        }
        int[] catalans = ((int[])(new int[]{1}));
        int n = 1;
        while (n <= upper_limit) {
            int next_val = 0;
            int j = 0;
            while (j < n) {
                next_val = next_val + catalans[j] * catalans[n - j - 1];
                j = j + 1;
            }
            catalans = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(catalans), java.util.stream.IntStream.of(next_val)).toArray()));
            n = n + 1;
        }
        return catalans;
    }
    public static void main(String[] args) {
        System.out.println(_p(catalan_numbers(5)));
        System.out.println(_p(catalan_numbers(2)));
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
