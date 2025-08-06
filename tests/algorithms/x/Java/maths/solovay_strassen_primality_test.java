public class Main {
    static int seed = 0;

    static void set_seed(int s) {
        seed = s;
    }

    static int randint(int a, int b) {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return (Math.floorMod(seed, (b - a + 1))) + a;
    }

    static int jacobi_symbol(int random_a, int number) {
        if (random_a == 0 || random_a == 1) {
            return random_a;
        }
        random_a = Math.floorMod(random_a, number);
        int t = 1;
        while (random_a != 0) {
            while (Math.floorMod(random_a, 2) == 0) {
                random_a = random_a / 2;
                int r = Math.floorMod(number, 8);
                if (r == 3 || r == 5) {
                    t = -t;
                }
            }
            int temp = random_a;
            random_a = number;
            number = temp;
            if (Math.floorMod(random_a, 4) == 3 && Math.floorMod(number, 4) == 3) {
                t = -t;
            }
            random_a = Math.floorMod(random_a, number);
        }
        if (number == 1) {
            return t;
        }
        return 0;
    }

    static int pow_mod(int base, int exp, int mod) {
        int result = 1;
        int b = Math.floorMod(base, mod);
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = Math.floorMod((result * b), mod);
            }
            b = Math.floorMod((b * b), mod);
            e = e / 2;
        }
        return result;
    }

    static boolean solovay_strassen(int number, int iterations) {
        if (number <= 1) {
            return false;
        }
        if (number <= 3) {
            return true;
        }
        int i = 0;
        while (i < iterations) {
            int a = randint(2, number - 2);
            int x = jacobi_symbol(a, number);
            int y = pow_mod(a, (number - 1) / 2, number);
            int mod_x = Math.floorMod(x, number);
            if (mod_x < 0) {
                mod_x = mod_x + number;
            }
            if (x == 0 || y != mod_x) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static void main() {
        set_seed(10);
        System.out.println(_p(solovay_strassen(13, 5)));
        System.out.println(_p(solovay_strassen(9, 10)));
        System.out.println(_p(solovay_strassen(17, 15)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 1;
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
