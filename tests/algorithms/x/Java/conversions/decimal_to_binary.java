public class Main {

    static String decimal_to_binary_iterative(int num) {
        if (num == 0) {
            return "0b0";
        }
        boolean negative = false;
        int n = num;
        if (n < 0) {
            negative = true;
            n = -n;
        }
        String result = "";
        while (n > 0) {
            result = _p(Math.floorMod(n, 2)) + result;
            n = n / 2;
        }
        if (negative) {
            return "-0b" + result;
        }
        return "0b" + result;
    }

    static String decimal_to_binary_recursive_helper(int n) {
        if (n == 0) {
            return "0";
        }
        if (n == 1) {
            return "1";
        }
        int div = n / 2;
        int mod = Math.floorMod(n, 2);
        return String.valueOf(decimal_to_binary_recursive_helper(div)) + _p(mod);
    }

    static String decimal_to_binary_recursive(int num) {
        if (num == 0) {
            return "0b0";
        }
        if (num < 0) {
            return "-0b" + String.valueOf(decimal_to_binary_recursive_helper(-num));
        }
        return "0b" + String.valueOf(decimal_to_binary_recursive_helper(num));
    }
    public static void main(String[] args) {
        System.out.println(decimal_to_binary_iterative(0));
        System.out.println(decimal_to_binary_iterative(2));
        System.out.println(decimal_to_binary_iterative(7));
        System.out.println(decimal_to_binary_iterative(35));
        System.out.println(decimal_to_binary_iterative(-2));
        System.out.println(decimal_to_binary_recursive(0));
        System.out.println(decimal_to_binary_recursive(40));
        System.out.println(decimal_to_binary_recursive(-40));
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
