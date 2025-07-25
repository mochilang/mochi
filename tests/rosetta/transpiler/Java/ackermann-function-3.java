public class Main {
    static String err = "";

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

    static int bit_len(java.math.BigInteger x) {
        java.math.BigInteger n = x;
        int c = 0;
        while (n.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            n = n.divide(java.math.BigInteger.valueOf(2));
            c = c + 1;
        }
        return c;
    }

    static java.math.BigInteger ackermann2(java.math.BigInteger m, java.math.BigInteger n) {
        if (!(err.equals(""))) {
            return java.math.BigInteger.valueOf(0);
        }
        if (m.compareTo(java.math.BigInteger.valueOf(3)) <= 0) {
            int mi = ((Number)(m)).intValue();
            if (mi == 0) {
                return n.add(java.math.BigInteger.valueOf(1));
            }
            if (mi == 1) {
                return n.add(java.math.BigInteger.valueOf(2));
            }
            if (mi == 2) {
                return java.math.BigInteger.valueOf(2).multiply(n).add(java.math.BigInteger.valueOf(3));
            }
            if (mi == 3) {
                int nb = bit_len(n);
                if (nb > 64) {
                    err = "A(m,n) had n of " + String.valueOf(nb) + " bits; too large";
                    return java.math.BigInteger.valueOf(0);
                }
                java.math.BigInteger r = pow_big(java.math.BigInteger.valueOf(2), ((Number)(n)).intValue());
                return java.math.BigInteger.valueOf(8).multiply(r).subtract(java.math.BigInteger.valueOf(3));
            }
        }
        if (bit_len(n) == 0) {
            return ackermann2(m.subtract((java.math.BigInteger.valueOf(1))), java.math.BigInteger.valueOf(1));
        }
        return ackermann2(m.subtract((java.math.BigInteger.valueOf(1))), ackermann2(m, n.subtract((java.math.BigInteger.valueOf(1)))));
    }

    static void show(int m, int n) {
        err = "";
        java.math.BigInteger res = ackermann2(new java.math.BigInteger(String.valueOf(m)), new java.math.BigInteger(String.valueOf(n)));
        if (!(err.equals(""))) {
            System.out.println("A(" + String.valueOf(m) + ", " + String.valueOf(n) + ") = Error: " + err);
            return;
        }
        if (bit_len(res) <= 256) {
            System.out.println("A(" + String.valueOf(m) + ", " + String.valueOf(n) + ") = " + String.valueOf(res));
        } else {
            String s = String.valueOf(res);
            String pre = s.substring(0, 20);
            String suf = s.substring(s.length() - 20, s.length());
            System.out.println("A(" + String.valueOf(m) + ", " + String.valueOf(n) + ") = " + String.valueOf(s.length()) + " digits starting/ending with: " + pre + "..." + suf);
        }
    }

    static void main() {
        show(0, 0);
        show(1, 2);
        show(2, 4);
        show(3, 100);
        show(3, 1000000);
        show(4, 1);
        show(4, 2);
        show(4, 3);
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
