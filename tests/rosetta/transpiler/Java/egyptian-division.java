public class Main {
    static class DivResult {
        int q;
        int r;
        DivResult(int q, int r) {
            this.q = q;
            this.r = r;
        }
        @Override public String toString() {
            return String.format("{'q': %s, 'r': %s}", String.valueOf(q), String.valueOf(r));
        }
    }


    static DivResult egyptianDivide(int dividend, int divisor) {
        if (dividend < 0 || divisor <= 0) {
            throw new RuntimeException(String.valueOf("Invalid argument(s)"));
        }
        if (dividend < divisor) {
            return new DivResult(0, dividend);
        }
        int[] powers = new int[]{1};
        int[] doublings = new int[]{divisor};
        int doubling = divisor * 2;
        while (doubling <= dividend) {
            powers = java.util.stream.IntStream.concat(java.util.Arrays.stream(powers), java.util.stream.IntStream.of(powers[powers.length - 1] * 2)).toArray();
            doublings = java.util.stream.IntStream.concat(java.util.Arrays.stream(doublings), java.util.stream.IntStream.of(doubling)).toArray();
            doubling = doubling * 2;
        }
        int ans = 0;
        int accum = 0;
        int i = doublings.length - 1;
        while (i >= 0) {
            if (accum + doublings[i] <= dividend) {
                accum = accum + doublings[i];
                ans = ans + powers[i];
                if (accum == dividend) {
                    break;
                }
            }
            i = i - 1;
        }
        return new DivResult(ans, dividend - accum);
    }

    static void main() {
        int dividend = 580;
        int divisor = 34;
        DivResult res = egyptianDivide(dividend, divisor);
        System.out.println(String.valueOf(dividend) + " divided by " + String.valueOf(divisor) + " is " + String.valueOf(res.q) + " with remainder " + String.valueOf(res.r));
    }
    public static void main(String[] args) {
        main();
    }
}
