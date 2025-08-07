public class Main {

    static boolean is_palindrome_str(String s) {
        int i = 0;
        int j = _runeLen(s) - 1;
        while (i < j) {
            if (!(_substr(s, i, i + 1).equals(_substr(s, j, j + 1)))) {
                return false;
            }
            i = i + 1;
            j = j - 1;
        }
        return true;
    }

    static String to_binary(int n) {
        if (n == 0) {
            return "0";
        }
        String res = "";
        int x = n;
        while (x > 0) {
            res = _p(Math.floorMod(x, 2)) + res;
            x = Math.floorDiv(x, 2);
        }
        return res;
    }

    static int solution(int n) {
        int total = 0;
        int i_1 = 1;
        while (i_1 < n) {
            String dec = _p(i_1);
            String bin = String.valueOf(to_binary(i_1));
            if (((Boolean)(is_palindrome_str(dec))) && ((Boolean)(is_palindrome_str(bin)))) {
                total = total + i_1;
            }
            i_1 = i_1 + 1;
        }
        return total;
    }
    public static void main(String[] args) {
        System.out.println(solution(1000000));
        System.out.println(solution(500000));
        System.out.println(solution(100000));
        System.out.println(solution(1000));
        System.out.println(solution(100));
        System.out.println(solution(10));
        System.out.println(solution(2));
        System.out.println(solution(1));
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
