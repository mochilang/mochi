public class Main {

    static String get_reverse_bit_string(int number) {
        String bit_string = "";
        int n = number;
        int i = 0;
        while (i < 32) {
            bit_string = bit_string + _p(Math.floorMod(n, 2));
            n = n / 2;
            i = i + 1;
        }
        return bit_string;
    }

    static String reverse_bit(int number) {
        if (number < 0) {
            throw new RuntimeException(String.valueOf("the value of input must be positive"));
        }
        int n_1 = number;
        int result = 0;
        int i_1 = 1;
        while (i_1 <= 32) {
            result = result * 2;
            int end_bit = Math.floorMod(n_1, 2);
            n_1 = n_1 / 2;
            result = result + end_bit;
            i_1 = i_1 + 1;
        }
        return get_reverse_bit_string(result);
    }
    public static void main(String[] args) {
        System.out.println(reverse_bit(25));
        System.out.println(reverse_bit(37));
        System.out.println(reverse_bit(21));
        System.out.println(reverse_bit(58));
        System.out.println(reverse_bit(0));
        System.out.println(reverse_bit(256));
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
