public class Main {

    static boolean is_palindrome(int[] values) {
        int[] stack = ((int[])(new int[]{}));
        int fast = 0;
        int slow = 0;
        int n = values.length;
        while (fast < n && fast + 1 < n) {
            stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(values[slow])).toArray()));
            slow = slow + 1;
            fast = fast + 2;
        }
        if (fast == n - 1) {
            slow = slow + 1;
        }
        int i = stack.length - 1;
        while (slow < n) {
            if (stack[i] != values[slow]) {
                return false;
            }
            i = i - 1;
            slow = slow + 1;
        }
        return true;
    }

    static void main() {
        System.out.println(is_palindrome(((int[])(new int[]{}))));
        System.out.println(is_palindrome(((int[])(new int[]{1}))));
        System.out.println(is_palindrome(((int[])(new int[]{1, 2}))));
        System.out.println(is_palindrome(((int[])(new int[]{1, 2, 1}))));
        System.out.println(is_palindrome(((int[])(new int[]{1, 2, 2, 1}))));
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
