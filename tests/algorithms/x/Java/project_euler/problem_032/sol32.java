public class Main {
    static String[] digits;
    static int[] products = new int[0];
    static int total = 0;
    static int i_4 = 0;

    static String join_digits(String[] xs) {
        String s = "";
        int i = 0;
        while (i < xs.length) {
            s = s + xs[i];
            i = i + 1;
        }
        return s;
    }

    static int digits_to_int(String[] xs) {
        return ((Number)(join_digits(((String[])(xs))))).intValue();
    }

    static boolean contains_int(int[] xs, int value) {
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (xs[i_1] == value) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static String[] remove_at(String[] xs, int idx) {
        String[] res = ((String[])(new String[]{}));
        int i_2 = 0;
        while (i_2 < xs.length) {
            if (i_2 != idx) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(xs[i_2])).toArray(String[]::new)));
            }
            i_2 = i_2 + 1;
        }
        return res;
    }

    static boolean is_combination_valid(String[] comb) {
        int prod = digits_to_int(((String[])(java.util.Arrays.copyOfRange(comb, 5, 9))));
        int mul2 = digits_to_int(((String[])(java.util.Arrays.copyOfRange(comb, 0, 2))));
        int mul3 = digits_to_int(((String[])(java.util.Arrays.copyOfRange(comb, 2, 5))));
        if (mul2 * mul3 == prod) {
            return true;
        }
        int mul1 = digits_to_int(((String[])(java.util.Arrays.copyOfRange(comb, 0, 1))));
        int mul4 = digits_to_int(((String[])(java.util.Arrays.copyOfRange(comb, 1, 5))));
        return mul1 * mul4 == prod;
    }

    static int[] search(String[] prefix, String[] remaining, int[] products) {
        if (remaining.length == 0) {
            if (((Boolean)(is_combination_valid(((String[])(prefix)))))) {
                int p = digits_to_int(((String[])(java.util.Arrays.copyOfRange(prefix, 5, 9))));
                if (!(Boolean)contains_int(((int[])(products)), p)) {
                    products = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(products), java.util.stream.IntStream.of(p)).toArray()));
                }
            }
            return products;
        }
        int i_3 = 0;
        while (i_3 < remaining.length) {
            String[] next_prefix = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(prefix), java.util.stream.Stream.of(remaining[i_3])).toArray(String[]::new)));
            String[] next_remaining = ((String[])(remove_at(((String[])(remaining)), i_3)));
            products = ((int[])(search(((String[])(next_prefix)), ((String[])(next_remaining)), ((int[])(products)))));
            i_3 = i_3 + 1;
        }
        return products;
    }
    public static void main(String[] args) {
        digits = ((String[])(new String[]{"1", "2", "3", "4", "5", "6", "7", "8", "9"}));
        products = ((int[])(new int[]{}));
        products = ((int[])(search(((String[])(new String[]{})), ((String[])(digits)), ((int[])(products)))));
        total = 0;
        i_4 = 0;
        while (i_4 < products.length) {
            total = total + products[i_4];
            i_4 = i_4 + 1;
        }
        System.out.println(_p(total));
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
