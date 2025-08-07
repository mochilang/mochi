public class Main {

    static int count_divisors(int n) {
        int m = n;
        int n_divisors = 1;
        int i = 2;
        while (i * i <= m) {
            int multiplicity = 0;
            while (Math.floorMod(m, i) == 0) {
                m = Math.floorDiv(m, i);
                multiplicity = multiplicity + 1;
            }
            n_divisors = n_divisors * (multiplicity + 1);
            i = i + 1;
        }
        if (m > 1) {
            n_divisors = n_divisors * 2;
        }
        return n_divisors;
    }

    static int solution() {
        int t_num = 1;
        int i_1 = 1;
        while (true) {
            i_1 = i_1 + 1;
            t_num = t_num + i_1;
            if (count_divisors(t_num) > 500) {
                break;
            }
        }
        return t_num;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution()));
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
