public class Main {

    static int countDivisors(int n) {
        if (n < 2) {
            return 1;
        }
        int count = 2;
        int i = 2;
        while (i <= n / 2) {
            if (Math.floorMod(n, i) == 0) {
                count = count + 1;
            }
            i = i + 1;
        }
        return count;
    }

    static void main() {
        System.out.println("The first 20 anti-primes are:");
        int maxDiv = 0;
        int count_1 = 0;
        int n = 1;
        String line = "";
        while (count_1 < 20) {
            int d = countDivisors(n);
            if (d > maxDiv) {
                line = line + _p(n) + " ";
                maxDiv = d;
                count_1 = count_1 + 1;
            }
            n = n + 1;
        }
        line = _substr(line, 0, _runeLen(line) - 1);
        System.out.println(line);
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
