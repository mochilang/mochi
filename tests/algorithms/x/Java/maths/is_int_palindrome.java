public class Main {

    static boolean is_int_palindrome(int num) {
        if (num < 0) {
            return false;
        }
        int n = num;
        int rev = 0;
        while (n > 0) {
            rev = rev * 10 + (Math.floorMod(n, 10));
            n = n / 10;
        }
        return rev == num;
    }

    static void main() {
        System.out.println(is_int_palindrome(-121));
        System.out.println(is_int_palindrome(0));
        System.out.println(is_int_palindrome(10));
        System.out.println(is_int_palindrome(11));
        System.out.println(is_int_palindrome(101));
        System.out.println(is_int_palindrome(120));
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
