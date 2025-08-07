public class Main {

    static boolean is_automorphic_number(int number) {
        if (number < 0) {
            return false;
        }
        int n = number;
        int sq = number * number;
        while (n > 0) {
            if (Math.floorMod(n, 10) != Math.floorMod(sq, 10)) {
                return false;
            }
            n = Math.floorDiv(n, 10);
            sq = Math.floorDiv(sq, 10);
        }
        return true;
    }
    public static void main(String[] args) {
        System.out.println(_p(is_automorphic_number(0)));
        System.out.println(_p(is_automorphic_number(1)));
        System.out.println(_p(is_automorphic_number(5)));
        System.out.println(_p(is_automorphic_number(6)));
        System.out.println(_p(is_automorphic_number(7)));
        System.out.println(_p(is_automorphic_number(25)));
        System.out.println(_p(is_automorphic_number(376)));
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
