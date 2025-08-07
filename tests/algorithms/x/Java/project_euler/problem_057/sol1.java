public class Main {

    static int solution(int n) {
        int prev_num = 1;
        int prev_den = 1;
        int count = 0;
        int i = 1;
        while (i <= n) {
            int num = prev_num + 2 * prev_den;
            int den = prev_num + prev_den;
            if (_runeLen(_p(num)) > _runeLen(_p(den))) {
                count = count + 1;
            }
            prev_num = num;
            prev_den = den;
            i = i + 1;
        }
        return count;
    }
    public static void main(String[] args) {
        System.out.println(solution(14));
        System.out.println(solution(100));
        System.out.println(solution(1000));
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
