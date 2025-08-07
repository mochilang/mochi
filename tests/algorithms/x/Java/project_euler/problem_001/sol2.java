public class Main {

    static int sum_of_multiples(int n) {
        int total = 0;
        int terms = Math.floorDiv((n - 1), 3);
        total = total + Math.floorDiv((terms * (6 + (terms - 1) * 3)), 2);
        terms = Math.floorDiv((n - 1), 5);
        total = total + Math.floorDiv((terms * (10 + (terms - 1) * 5)), 2);
        terms = Math.floorDiv((n - 1), 15);
        total = total - Math.floorDiv((terms * (30 + (terms - 1) * 15)), 2);
        return total;
    }
    public static void main(String[] args) {
        System.out.println("solution() = " + _p(sum_of_multiples(1000)));
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
