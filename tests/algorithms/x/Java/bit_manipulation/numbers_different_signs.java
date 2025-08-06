public class Main {

    static boolean different_signs(int num1, int num2) {
        boolean sign1 = num1 < 0;
        boolean sign2 = num2 < 0;
        return sign1 != sign2;
    }
    public static void main(String[] args) {
        System.out.println(_p(different_signs(1, -1)));
        System.out.println(_p(different_signs(1, 1)));
        System.out.println(_p(different_signs((int)1000000000000000000L, -(int)1000000000000000000L)));
        System.out.println(_p(different_signs(-(int)1000000000000000000L, (int)1000000000000000000L)));
        System.out.println(_p(different_signs(50, 278)));
        System.out.println(_p(different_signs(0, 2)));
        System.out.println(_p(different_signs(2, 0)));
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
