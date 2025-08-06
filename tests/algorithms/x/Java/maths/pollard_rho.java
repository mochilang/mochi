public class Main {
    static class PollardResult {
        int factor;
        boolean ok;
        PollardResult(int factor, boolean ok) {
            this.factor = factor;
            this.ok = ok;
        }
        PollardResult() {}
        @Override public String toString() {
            return String.format("{'factor': %s, 'ok': %s}", String.valueOf(factor), String.valueOf(ok));
        }
    }


    static int gcd(int a, int b) {
        int x = a < 0 ? -a : a;
        int y = b < 0 ? -b : b;
        while (y != 0) {
            int t = Math.floorMod(x, y);
            x = y;
            y = t;
        }
        return x;
    }

    static int rand_fn(int value, int step, int modulus) {
        return Math.floorMod((value * value + step), modulus);
    }

    static PollardResult pollard_rho(int num, int seed, int step, int attempts) {
        if (num < 2) {
            throw new RuntimeException(String.valueOf("The input value cannot be less than 2"));
        }
        if (num > 2 && Math.floorMod(num, 2) == 0) {
            return new PollardResult(2, true);
        }
        int s = seed;
        int st = step;
        int i = 0;
        while (i < attempts) {
            int tortoise = s;
            int hare = s;
            while (true) {
                tortoise = rand_fn(tortoise, st, num);
                hare = rand_fn(hare, st, num);
                hare = rand_fn(hare, st, num);
                int divisor = gcd(hare - tortoise, num);
                if (divisor == 1) {
                    continue;
                } else                 if (divisor == num) {
                    break;
                } else {
                    return new PollardResult(divisor, true);
                }
            }
            s = hare;
            st = st + 1;
            i = i + 1;
        }
        return new PollardResult(0, false);
    }

    static void test_pollard_rho() {
        PollardResult r1 = pollard_rho(8051, 2, 1, 5);
        if (!r1.ok || (r1.factor != 83 && r1.factor != 97)) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        PollardResult r2 = pollard_rho(10403, 2, 1, 5);
        if (!r2.ok || (r2.factor != 101 && r2.factor != 103)) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        PollardResult r3 = pollard_rho(100, 2, 1, 3);
        if (!r3.ok || r3.factor != 2) {
            throw new RuntimeException(String.valueOf("test3 failed"));
        }
        PollardResult r4 = pollard_rho(17, 2, 1, 3);
        if (r4.ok) {
            throw new RuntimeException(String.valueOf("test4 failed"));
        }
        PollardResult r5 = pollard_rho(17 * 17 * 17, 2, 1, 3);
        if (!r5.ok || r5.factor != 17) {
            throw new RuntimeException(String.valueOf("test5 failed"));
        }
        PollardResult r6 = pollard_rho(17 * 17 * 17, 2, 1, 1);
        if (r6.ok) {
            throw new RuntimeException(String.valueOf("test6 failed"));
        }
        PollardResult r7 = pollard_rho(3 * 5 * 7, 2, 1, 3);
        if (!r7.ok || r7.factor != 21) {
            throw new RuntimeException(String.valueOf("test7 failed"));
        }
    }

    static void main() {
        test_pollard_rho();
        PollardResult a = pollard_rho(100, 2, 1, 3);
        if (a.ok) {
            System.out.println(_p(a.factor));
        } else {
            System.out.println("None");
        }
        PollardResult b = pollard_rho(17, 2, 1, 3);
        if (b.ok) {
            System.out.println(_p(b.factor));
        } else {
            System.out.println("None");
        }
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
