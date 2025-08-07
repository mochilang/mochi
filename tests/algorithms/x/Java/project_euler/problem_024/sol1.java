public class Main {

    static int factorial(int n) {
        int result = 1;
        int i = 2;
        while (i <= n) {
            result = result * i;
            i = i + 1;
        }
        return result;
    }

    static String nth_permutation(String digits, int index) {
        String chars = digits;
        int n = index;
        String res = "";
        int k = _runeLen(chars);
        while (k > 0) {
            int f = factorial(k - 1);
            int pos = Math.floorDiv(n, f);
            n = Math.floorMod(n, f);
            res = res + _substr(chars, pos, pos + 1);
            chars = _substr(chars, 0, pos) + _substr(chars, pos + 1, _runeLen(chars));
            k = k - 1;
        }
        return res;
    }

    static String solution() {
        return nth_permutation("0123456789", 999999);
    }

    static void main() {
        System.out.println(solution());
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
