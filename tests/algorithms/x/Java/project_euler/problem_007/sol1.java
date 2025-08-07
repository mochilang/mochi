public class Main {

    static int isqrt(int n) {
        int r = 0;
        while ((r + 1) * (r + 1) <= n) {
            r = r + 1;
        }
        return r;
    }

    static boolean is_prime(int number) {
        if (1 < number && number < 4) {
            return true;
        } else         if (number < 2 || Math.floorMod(number, 2) == 0 || Math.floorMod(number, 3) == 0) {
            return false;
        }
        int limit = isqrt(number);
        int i = 5;
        while (i <= limit) {
            if (Math.floorMod(number, i) == 0 || Math.floorMod(number, (i + 2)) == 0) {
                return false;
            }
            i = i + 6;
        }
        return true;
    }

    static int solution(int nth) {
        int count = 0;
        int number = 1;
        while (count != nth && number < 3) {
            number = number + 1;
            if (((Boolean)(is_prime(number)))) {
                count = count + 1;
            }
        }
        while (count != nth) {
            number = number + 2;
            if (((Boolean)(is_prime(number)))) {
                count = count + 1;
            }
        }
        return number;
    }

    static void main() {
        System.out.println("solution() = " + _p(solution(10001)));
    }
    public static void main(String[] args) {
        main();
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
