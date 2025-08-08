public class Main {

    static boolean isPrime(int number) {
        if (number < 2) {
            return false;
        }
        if (number < 4) {
            return true;
        }
        if (Math.floorMod(number, 2) == 0) {
            return false;
        }
        int i = 3;
        while (i * i <= number) {
            if (Math.floorMod(number, i) == 0) {
                return false;
            }
            i = i + 2;
        }
        return true;
    }

    static int nextPrime(int value, int factor, boolean desc) {
        int v = value * factor;
        int firstValue = v;
        while (!(Boolean)isPrime(v)) {
            if (((Boolean)(desc))) {
                v = v - 1;
            } else {
                v = v + 1;
            }
        }
        if (v == firstValue) {
            if (((Boolean)(desc))) {
                return nextPrime(v - 1, 1, desc);
            } else {
                return nextPrime(v + 1, 1, desc);
            }
        }
        return v;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(isPrime(0));
            System.out.println(isPrime(1));
            System.out.println(isPrime(2));
            System.out.println(isPrime(3));
            System.out.println(isPrime(27));
            System.out.println(isPrime(87));
            System.out.println(isPrime(563));
            System.out.println(isPrime(2999));
            System.out.println(isPrime(67483));
            System.out.println(nextPrime(14, 1, false));
            System.out.println(nextPrime(14, 1, true));
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
