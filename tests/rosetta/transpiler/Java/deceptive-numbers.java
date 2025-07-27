public class Main {

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0) {
            return n == 2;
        }
        if (Math.floorMod(n, 3) == 0) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 2;
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 4;
        }
        return true;
    }

    static String listToString(int[] xs) {
        String s = "[";
        int i = 0;
        while (i < xs.length) {
            s = s + String.valueOf(xs[i]);
            if (i < xs.length - 1) {
                s = s + " ";
            }
            i = i + 1;
        }
        return s + "]";
    }

    static void main() {
        int count = 0;
        int limit = 25;
        int n = 17;
        java.math.BigInteger repunit = java.math.BigInteger.valueOf((int)1111111111111111L);
        java.math.BigInteger eleven = java.math.BigInteger.valueOf(11);
        java.math.BigInteger hundred = java.math.BigInteger.valueOf(100);
        int[] deceptive = new int[]{};
        while (count < limit) {
            if (!(Boolean)isPrime(n) && Math.floorMod(n, 3) != 0 && Math.floorMod(n, 5) != 0) {
                java.math.BigInteger bn = new java.math.BigInteger(String.valueOf(n));
                if (repunit.remainder(bn).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                    deceptive = java.util.stream.IntStream.concat(java.util.Arrays.stream(deceptive), java.util.stream.IntStream.of(n)).toArray();
                    count = count + 1;
                }
            }
            n = n + 2;
            repunit = (repunit.multiply(hundred)).add(eleven);
        }
        System.out.println("The first " + String.valueOf(limit) + " deceptive numbers are:");
        System.out.println(listToString(deceptive));
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
