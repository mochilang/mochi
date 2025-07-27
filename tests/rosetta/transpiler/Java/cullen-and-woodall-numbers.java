public class Main {

    static java.math.BigInteger pow_big(java.math.BigInteger base, int exp) {
        java.math.BigInteger result = java.math.BigInteger.valueOf(1);
        java.math.BigInteger b = base;
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = result.multiply(b);
            }
            b = b.multiply(b);
            e = ((Number)((e / 2))).intValue();
        }
        return result;
    }

    static java.math.BigInteger cullen(int n) {
        java.math.BigInteger two_n = pow_big(java.math.BigInteger.valueOf(2), n);
        return (two_n.multiply((new java.math.BigInteger(String.valueOf(n))))).add((java.math.BigInteger.valueOf(1)));
    }

    static java.math.BigInteger woodall(int n) {
        return cullen(n).subtract((java.math.BigInteger.valueOf(2)));
    }

    static String show_list(java.math.BigInteger[] xs) {
        String line = "";
        int i = 0;
        while (i < xs.length) {
            line = line + String.valueOf(xs[i]);
            if (i < xs.length - 1) {
                line = line + " ";
            }
            i = i + 1;
        }
        return line;
    }

    static void main() {
        java.math.BigInteger[] cnums = new java.math.BigInteger[]{};
        int i = 1;
        while (i <= 20) {
            cnums = java.util.stream.Stream.concat(java.util.Arrays.stream(cnums), java.util.stream.Stream.of(cullen(i))).toArray(java.math.BigInteger[]::new);
            i = i + 1;
        }
        System.out.println("First 20 Cullen numbers (n * 2^n + 1):");
        System.out.println(show_list(cnums));
        java.math.BigInteger[] wnums = new java.math.BigInteger[]{};
        i = 1;
        while (i <= 20) {
            wnums = java.util.stream.Stream.concat(java.util.Arrays.stream(wnums), java.util.stream.Stream.of(woodall(i))).toArray(java.math.BigInteger[]::new);
            i = i + 1;
        }
        System.out.println("\nFirst 20 Woodall numbers (n * 2^n - 1):");
        System.out.println(show_list(wnums));
        java.math.BigInteger[] cprimes = new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(141), java.math.BigInteger.valueOf(4713), java.math.BigInteger.valueOf(5795), java.math.BigInteger.valueOf(6611)};
        System.out.println("\nFirst 5 Cullen primes (in terms of n):");
        System.out.println(show_list(cprimes));
        java.math.BigInteger[] wprimes = new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(30), java.math.BigInteger.valueOf(75), java.math.BigInteger.valueOf(81), java.math.BigInteger.valueOf(115), java.math.BigInteger.valueOf(123), java.math.BigInteger.valueOf(249), java.math.BigInteger.valueOf(362), java.math.BigInteger.valueOf(384), java.math.BigInteger.valueOf(462)};
        System.out.println("\nFirst 12 Woodall primes (in terms of n):");
        System.out.println(show_list(wprimes));
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
