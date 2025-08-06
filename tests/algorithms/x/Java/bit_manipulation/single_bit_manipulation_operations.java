public class Main {

    static int pow2(int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * 2;
            i = i + 1;
        }
        return result;
    }

    static boolean is_bit_set(int number, int position) {
        int shifted = number / pow2(position);
        int remainder = Math.floorMod(shifted, 2);
        return remainder == 1;
    }

    static int set_bit(int number, int position) {
        if (((Boolean)(is_bit_set(number, position)))) {
            return number;
        }
        return number + pow2(position);
    }

    static int clear_bit(int number, int position) {
        if (((Boolean)(is_bit_set(number, position)))) {
            return number - pow2(position);
        }
        return number;
    }

    static int flip_bit(int number, int position) {
        if (((Boolean)(is_bit_set(number, position)))) {
            return number - pow2(position);
        }
        return number + pow2(position);
    }

    static int get_bit(int number, int position) {
        if (((Boolean)(is_bit_set(number, position)))) {
            return 1;
        }
        return 0;
    }
    public static void main(String[] args) {
        System.out.println(_p(set_bit(13, 1)));
        System.out.println(_p(clear_bit(18, 1)));
        System.out.println(_p(flip_bit(5, 1)));
        System.out.println(_p(is_bit_set(10, 3)));
        System.out.println(_p(get_bit(10, 1)));
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
