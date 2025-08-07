public class Main {

    static int next_number(int number) {
        int n = number;
        int total = 0;
        while (n > 0) {
            int d = Math.floorMod(n, 10);
            total = total + d * d;
            n = Math.floorDiv(n, 10);
        }
        return total;
    }

    static boolean chain(int number) {
        int n_1 = number;
        while (n_1 != 1 && n_1 != 89) {
            n_1 = next_number(n_1);
        }
        return n_1 == 1;
    }

    static int solution(int limit) {
        int count = 0;
        int i = 1;
        while (i < limit) {
            if (!(Boolean)chain(i)) {
                count = count + 1;
            }
            i = i + 1;
        }
        return count;
    }
    public static void main(String[] args) {
        System.out.println(_p(next_number(44)));
        System.out.println(_p(next_number(10)));
        System.out.println(_p(next_number(32)));
        System.out.println(_p(chain(10)));
        System.out.println(_p(chain(58)));
        System.out.println(_p(chain(1)));
        System.out.println(_p(solution(100)));
        System.out.println(_p(solution(1000)));
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
