public class Main {

    static int solution(int n) {
        int[] ones_counts = ((int[])(new int[]{0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8}));
        int[] tens_counts = ((int[])(new int[]{0, 0, 6, 6, 5, 5, 5, 7, 6, 6}));
        int count = 0;
        int i = 1;
        while (i <= n) {
            if (i < 1000) {
                if (i >= 100) {
                    count = count + ones_counts[Math.floorDiv(i, 100)] + 7;
                    if (Math.floorMod(i, 100) != 0) {
                        count = count + 3;
                    }
                }
                int remainder = Math.floorMod(i, 100);
                if (remainder > 0 && remainder < 20) {
                    count = count + ones_counts[remainder];
                } else {
                    count = count + ones_counts[Math.floorMod(i, 10)];
                    count = count + tens_counts[Math.floorDiv((remainder - Math.floorMod(i, 10)), 10)];
                }
            } else {
                count = count + ones_counts[Math.floorDiv(i, 1000)] + 8;
            }
            i = i + 1;
        }
        return count;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(1000)));
        System.out.println(_p(solution(5)));
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
