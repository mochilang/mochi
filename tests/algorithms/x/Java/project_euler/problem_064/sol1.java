public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int intSqrt(int n) {
        if (n == 0) {
            return 0;
        }
        int x = n;
        int y = Math.floorDiv((x + 1), 2);
        while (y < x) {
            x = y;
            y = Math.floorDiv((x + Math.floorDiv(n, x)), 2);
        }
        return x;
    }

    static int continuousFractionPeriod(int n) {
        int m = 0;
        int d = 1;
        int a0 = intSqrt(n);
        int a = a0;
        int period = 0;
        while (a != 2 * a0) {
            m = d * a - m;
            d = Math.floorDiv((n - m * m), d);
            a = Math.floorDiv((a0 + m), d);
            period = period + 1;
        }
        return period;
    }

    static int solution(int n) {
        int count = 0;
        for (int i = 2; i < (n + 1); i++) {
            int r = intSqrt(i);
            if (r * r != i) {
                int p = continuousFractionPeriod(i);
                if (Math.floorMod(p, 2) == 1) {
                    count = count + 1;
                }
            }
        }
        return count;
    }

    static void main() {
        String nStr = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        int n = Integer.parseInt(nStr);
        System.out.println(solution(n));
    }
    public static void main(String[] args) {
        main();
    }
}
