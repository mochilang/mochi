public class Main {

    static int largest_prime_factor(int n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Parameter n must be greater than or equal to one."));
        }
        int num = n;
        int prime = 1;
        int i = 2;
        while (i * i <= num) {
            while (Math.floorMod(num, i) == 0) {
                prime = i;
                num = Math.floorDiv(num, i);
            }
            i = i + 1;
        }
        if (num > 1) {
            prime = num;
        }
        return prime;
    }

    static void main() {
        System.out.println(_p(largest_prime_factor((int)600851475143L)));
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
