public class Main {
    static int ans;

    static boolean is_prime(int number) {
        if (number > 1 && number < 4) {
            return true;
        } else         if (number < 2 || Math.floorMod(number, 2) == 0 || Math.floorMod(number, 3) == 0) {
            return false;
        }
        int i = 5;
        while (i * i <= number) {
            if (Math.floorMod(number, i) == 0 || Math.floorMod(number, (i + 2)) == 0) {
                return false;
            }
            i = i + 6;
        }
        return true;
    }

    static int solution(int nth) {
        if (nth <= 0) {
            throw new RuntimeException(String.valueOf("Parameter nth must be greater than or equal to one."));
        }
        int[] primes = ((int[])(new int[]{}));
        int num = 2;
        while (primes.length < nth) {
            if (((Boolean)(is_prime(num)))) {
                primes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(primes), java.util.stream.IntStream.of(num)).toArray()));
            }
            num = num + 1;
        }
        return primes[primes.length - 1];
    }
    public static void main(String[] args) {
        ans = solution(10001);
        System.out.println("solution(10001) = " + _p(ans));
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
