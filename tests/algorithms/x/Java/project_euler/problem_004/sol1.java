public class Main {

    static boolean is_palindrome(int num) {
        String s = _p(num);
        int i = 0;
        int j = _runeLen(s) - 1;
        while (i < j) {
            if (!(s.substring(i, i + 1).equals(s.substring(j, j + 1)))) {
                return false;
            }
            i = i + 1;
            j = j - 1;
        }
        return true;
    }

    static int solution(int n) {
        int number = n - 1;
        while (number > 9999) {
            if (((Boolean)(is_palindrome(number)))) {
                int divisor = 999;
                while (divisor > 99) {
                    if (Math.floorMod(number, divisor) == 0) {
                        int other = Math.floorDiv(number, divisor);
                        if (_runeLen(_p(other)) == 3) {
                            return number;
                        }
                    }
                    divisor = divisor - 1;
                }
            }
            number = number - 1;
        }
        System.out.println("That number is larger than our acceptable range.");
        return 0;
    }
    public static void main(String[] args) {
        System.out.println("solution() = " + _p(solution(998001)));
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
