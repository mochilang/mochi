public class Main {

    static boolean is_happy_number(int num) {
        if (num <= 0) {
            throw new RuntimeException(String.valueOf("num must be a positive integer"));
        }
        int[] seen = ((int[])(new int[]{}));
        int n = num;
        while (n != 1) {
            int i = 0;
            while (i < seen.length) {
                if (seen[i] == n) {
                    return false;
                }
                i = i + 1;
            }
            seen = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(seen), java.util.stream.IntStream.of(n)).toArray()));
            int total = 0;
            int temp = n;
            while (temp > 0) {
                int digit = Math.floorMod(temp, 10);
                total = total + digit * digit;
                temp = Math.floorDiv(temp, 10);
            }
            n = total;
        }
        return true;
    }

    static void test_is_happy_number() {
        if (!(Boolean)is_happy_number(19)) {
            throw new RuntimeException(String.valueOf("19 should be happy"));
        }
        if (((Boolean)(is_happy_number(2)))) {
            throw new RuntimeException(String.valueOf("2 should be unhappy"));
        }
        if (!(Boolean)is_happy_number(23)) {
            throw new RuntimeException(String.valueOf("23 should be happy"));
        }
        if (!(Boolean)is_happy_number(1)) {
            throw new RuntimeException(String.valueOf("1 should be happy"));
        }
    }

    static void main() {
        test_is_happy_number();
        System.out.println(is_happy_number(19));
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
