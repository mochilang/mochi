public class Main {

    static int pow(int base, int exp) {
        int result = 1;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static boolean isDisarium(int n) {
        int[] digits = new int[]{};
        int x = n;
        if (x == 0) {
            digits = java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(0)).toArray();
        }
        while (x > 0) {
            digits = java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(Math.floorMod(x, 10))).toArray();
            x = ((Number)((x / 10))).intValue();
        }
        int sum = 0;
        int pos = 1;
        int i = digits.length - 1;
        while (i >= 0) {
            sum = sum + pow(digits[i], pos);
            pos = pos + 1;
            i = i - 1;
        }
        return sum == n;
    }

    static void main() {
        int count = 0;
        int n = 0;
        while (count < 19 && n < 3000000) {
            if (isDisarium(n)) {
                System.out.println(String.valueOf(n));
                count = count + 1;
            }
            n = n + 1;
        }
        System.out.println("\nFound the first " + String.valueOf(count) + " Disarium numbers.");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
