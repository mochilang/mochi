public class Main {

    static boolean power_of_4(int number) {
        if (number <= 0) {
            return false;
        }
        int n = number;
        while (Math.floorMod(n, 4) == 0) {
            n = n / 4;
        }
        return n == 1;
    }
    public static void main(String[] args) {
        System.out.println(_p(power_of_4(1)));
        System.out.println(_p(power_of_4(2)));
        System.out.println(_p(power_of_4(4)));
        System.out.println(_p(power_of_4(6)));
        System.out.println(_p(power_of_4(64)));
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
