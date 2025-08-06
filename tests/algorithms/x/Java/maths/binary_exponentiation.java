public class Main {

    static double binary_exp_recursive(double base, int exponent) {
        if (exponent < 0) {
            throw new RuntimeException(String.valueOf("exponent must be non-negative"));
        }
        if (exponent == 0) {
            return 1.0;
        }
        if (Math.floorMod(exponent, 2) == 1) {
            return binary_exp_recursive(base, exponent - 1) * base;
        }
        double half = binary_exp_recursive(base, exponent / 2);
        return half * half;
    }

    static double binary_exp_iterative(double base, int exponent) {
        if (exponent < 0) {
            throw new RuntimeException(String.valueOf("exponent must be non-negative"));
        }
        double result = 1.0;
        double b = base;
        int e = exponent;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = result * b;
            }
            b = b * b;
            e = e / 2;
        }
        return result;
    }

    static int binary_exp_mod_recursive(int base, int exponent, int modulus) {
        if (exponent < 0) {
            throw new RuntimeException(String.valueOf("exponent must be non-negative"));
        }
        if (modulus <= 0) {
            throw new RuntimeException(String.valueOf("modulus must be positive"));
        }
        if (exponent == 0) {
            return Math.floorMod(1, modulus);
        }
        if (Math.floorMod(exponent, 2) == 1) {
            return Math.floorMod((binary_exp_mod_recursive(base, exponent - 1, modulus) * (Math.floorMod(base, modulus))), modulus);
        }
        int r = binary_exp_mod_recursive(base, exponent / 2, modulus);
        return Math.floorMod((r * r), modulus);
    }

    static int binary_exp_mod_iterative(int base, int exponent, int modulus) {
        if (exponent < 0) {
            throw new RuntimeException(String.valueOf("exponent must be non-negative"));
        }
        if (modulus <= 0) {
            throw new RuntimeException(String.valueOf("modulus must be positive"));
        }
        int result_1 = Math.floorMod(1, modulus);
        int b_1 = Math.floorMod(base, modulus);
        int e_1 = exponent;
        while (e_1 > 0) {
            if (Math.floorMod(e_1, 2) == 1) {
                result_1 = Math.floorMod((result_1 * b_1), modulus);
            }
            b_1 = Math.floorMod((b_1 * b_1), modulus);
            e_1 = e_1 / 2;
        }
        return result_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(binary_exp_recursive(3.0, 5));
            System.out.println(binary_exp_iterative(1.5, 4));
            System.out.println(binary_exp_mod_recursive(3, 4, 5));
            System.out.println(binary_exp_mod_iterative(11, 13, 7));
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
