public class Main {
    static int result;

    static boolean is_prime(int number) {
        if (number < 2) {
            return false;
        }
        if (number < 4) {
            return true;
        }
        if (Math.floorMod(number, 2) == 0 || Math.floorMod(number, 3) == 0) {
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

    static int solution(int n) {
        if (n <= 2) {
            return 0;
        }
        int total = 2;
        int num = 3;
        while (num < n) {
            if (((Boolean)(is_prime(num)))) {
                total = total + num;
            }
            num = num + 2;
        }
        return total;
    }
    public static void main(String[] args) {
        result = solution(1000);
        System.out.println("solution() = " + _p(result));
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
