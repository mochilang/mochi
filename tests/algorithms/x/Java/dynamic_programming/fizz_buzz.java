public class Main {

    static String fizz_buzz(int number, int iterations) {
        if (number < 1) {
            throw new RuntimeException(String.valueOf("starting number must be an integer and be more than 0"));
        }
        if (iterations < 1) {
            throw new RuntimeException(String.valueOf("Iterations must be done more than 0 times to play FizzBuzz"));
        }
        String out = "";
        int n = number;
        while (n <= iterations) {
            if (Math.floorMod(n, 3) == 0) {
                out = out + "Fizz";
            }
            if (Math.floorMod(n, 5) == 0) {
                out = out + "Buzz";
            }
            if (Math.floorMod(n, 3) != 0 && Math.floorMod(n, 5) != 0) {
                out = out + _p(n);
            }
            out = out + " ";
            n = n + 1;
        }
        return out;
    }
    public static void main(String[] args) {
        System.out.println(fizz_buzz(1, 7));
        System.out.println(fizz_buzz(1, 15));
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
