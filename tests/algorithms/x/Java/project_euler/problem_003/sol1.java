public class Main {

    static boolean is_prime(int number) {
        if (number > 1 && number < 4) {
            return true;
        }
        if (number < 2 || Math.floorMod(number, 2) == 0 || Math.floorMod(number, 3) == 0) {
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
        int num = n;
        if (num <= 0) {
            System.out.println("Parameter n must be greater than or equal to one.");
            return 0;
        }
        if (((Boolean)(is_prime(num)))) {
            return num;
        }
        while (Math.floorMod(num, 2) == 0) {
            num = Math.floorDiv(num, 2);
            if (((Boolean)(is_prime(num)))) {
                return num;
            }
        }
        int max_number = 1;
        int i_1 = 3;
        while (i_1 * i_1 <= num) {
            if (Math.floorMod(num, i_1) == 0) {
                if (((Boolean)(is_prime(Math.floorDiv(num, i_1))))) {
                    max_number = Math.floorDiv(num, i_1);
                    break;
                } else                 if (((Boolean)(is_prime(i_1)))) {
                    max_number = i_1;
                }
            }
            i_1 = i_1 + 2;
        }
        return max_number;
    }

    static void main() {
        int result = solution((int)600851475143L);
        System.out.println("solution() = " + _p(result));
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
