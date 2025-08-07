public class Main {

    static int solution(int n) {
        int total = 0;
        int num = 0;
        while (true) {
            num = num + 3;
            if (num >= n) {
                break;
            }
            total = total + num;
            num = num + 2;
            if (num >= n) {
                break;
            }
            total = total + num;
            num = num + 1;
            if (num >= n) {
                break;
            }
            total = total + num;
            num = num + 3;
            if (num >= n) {
                break;
            }
            total = total + num;
            num = num + 1;
            if (num >= n) {
                break;
            }
            total = total + num;
            num = num + 2;
            if (num >= n) {
                break;
            }
            total = total + num;
            num = num + 3;
            if (num >= n) {
                break;
            }
            total = total + num;
        }
        return total;
    }
    public static void main(String[] args) {
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
