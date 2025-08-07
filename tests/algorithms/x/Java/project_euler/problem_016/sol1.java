public class Main {

    static int power_of_two(int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * 2;
            i = i + 1;
        }
        return result;
    }

    static int solution(int power) {
        int num = power_of_two(power);
        String string_num = _p(num);
        int sum = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(string_num)) {
            sum = sum + (string_num.substring(i_1, i_1+1));
            i_1 = i_1 + 1;
        }
        return sum;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(1000)));
        System.out.println(_p(solution(50)));
        System.out.println(_p(solution(20)));
        System.out.println(_p(solution(15)));
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
