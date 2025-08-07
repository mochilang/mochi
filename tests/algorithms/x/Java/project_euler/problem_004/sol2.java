public class Main {

    static boolean is_palindrome(int num) {
        if (num < 0) {
            return false;
        }
        int n = num;
        int rev = 0;
        while (n > 0) {
            rev = rev * 10 + (Math.floorMod(n, 10));
            n = Math.floorDiv(n, 10);
        }
        return rev == num;
    }

    static int solution(int limit) {
        int answer = 0;
        int i = 999;
        while (i >= 100) {
            int j = 999;
            while (j >= 100) {
                int product = i * j;
                if (product < limit && ((Boolean)(is_palindrome(product))) && product > answer) {
                    answer = product;
                }
                j = j - 1;
            }
            i = i - 1;
        }
        return answer;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(998001)));
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
