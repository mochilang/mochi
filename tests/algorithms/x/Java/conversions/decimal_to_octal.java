public class Main {

    static int int_pow(int base, int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static String decimal_to_octal(int num) {
        if (num == 0) {
            return "0o0";
        }
        int octal = 0;
        int counter = 0;
        int value = num;
        while (value > 0) {
            int remainder = Math.floorMod(value, 8);
            octal = octal + remainder * int_pow(10, counter);
            counter = counter + 1;
            value = value / 8;
        }
        return "0o" + _p(octal);
    }
    public static void main(String[] args) {
        System.out.println(decimal_to_octal(2));
        System.out.println(decimal_to_octal(8));
        System.out.println(decimal_to_octal(65));
        System.out.println(decimal_to_octal(216));
        System.out.println(decimal_to_octal(512));
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
