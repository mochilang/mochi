public class Main {

    static int pow(int base, int exponent) {
        int result = 1;
        int i = 0;
        while (i < exponent) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static int num_digits(int n) {
        if (n == 0) {
            return 1;
        }
        int count = 0;
        int x = n;
        while (x > 0) {
            x = Math.floorDiv(x, 10);
            count = count + 1;
        }
        return count;
    }

    static int solution(int max_base, int max_power) {
        int total = 0;
        int base = 1;
        while (base < max_base) {
            int power = 1;
            while (power < max_power) {
                int digits = num_digits(pow(base, power));
                if (digits == power) {
                    total = total + 1;
                }
                power = power + 1;
            }
            base = base + 1;
        }
        return total;
    }
    public static void main(String[] args) {
        System.out.println("solution(10, 22) = " + _p(solution(10, 22)));
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
