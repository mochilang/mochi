public class Main {
    static int e1 = pow_int(3, 2);
    static int e2 = pow_int(4, e1);
    static java.math.BigInteger base = java.math.BigInteger.valueOf(5);
    static java.math.BigInteger x = pow_big(base, e2);
    static String s = String.valueOf(x);

    static int pow_int(int base, int exp) {
        int result = 1;
        int b = base;
        int e = exp;
        while (e > 0) {
            if (e % 2 == 1) {
                result = result * b;
            }
            b = b * b;
            e = ((Number)((e / 2))).intValue();
        }
        return result;
    }

    static java.math.BigInteger pow_big(java.math.BigInteger base, int exp) {
        java.math.BigInteger result = java.math.BigInteger.valueOf(1);
        java.math.BigInteger b = base;
        int e = exp;
        while (e > 0) {
            if (e % 2 == 1) {
                result = result.multiply(b);
            }
            b = b.multiply(b);
            e = ((Number)((e / 2))).intValue();
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf("5^(4^(3^2)) has" + " " + s.length()) + " ") + "digits:") + " ") + s.substring(0, 20)) + " ") + "...") + " ") + s.substring(s.length() - 20, s.length()));
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
