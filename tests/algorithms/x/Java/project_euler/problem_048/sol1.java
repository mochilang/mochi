public class Main {

    static int pow_mod(int base, int exponent, int modulus) {
        int result = 1;
        int b = Math.floorMod(base, modulus);
        int e = exponent;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = Math.floorMod((result * b), modulus);
            }
            b = Math.floorMod((b * b), modulus);
            e = Math.floorDiv(e, 2);
        }
        return result;
    }

    static String solution() {
        int modulus = (int)10000000000L;
        int total = 0;
        int i = 1;
        while (i <= 1000) {
            total = Math.floorMod((total + pow_mod(i, i, modulus)), modulus);
            i = i + 1;
        }
        String s = _p(total);
        while (_runeLen(s) < 10) {
            s = "0" + s;
        }
        return s;
    }
    public static void main(String[] args) {
        System.out.println(solution());
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
