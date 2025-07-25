public class Main {

    static int[] primeFactors(int n) {
        int[] factors = new int[]{};
        int x = n;
        while (x % 2 == 0) {
            factors = java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(2)).toArray();
            x = ((Number)((x / 2))).intValue();
        }
        int p = 3;
        while (p * p <= x) {
            while (x % p == 0) {
                factors = java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(p)).toArray();
                x = ((Number)((x / p))).intValue();
            }
            p = p + 2;
        }
        if (x > 1) {
            factors = java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(x)).toArray();
        }
        return factors;
    }

    static String repeat(String ch, int n) {
        String s = "";
        int i = 0;
        while (i < n) {
            s = String.valueOf(s + ch);
            i = i + 1;
        }
        return s;
    }

    static double D(double n) {
        if (n < 0.0) {
            return -D(-n);
        }
        if (n < 2.0) {
            return 0.0;
        }
        int[] factors = new int[]{};
        if (n < 10000000000000000000.0) {
            factors = primeFactors(((Number)((n))).intValue());
        } else {
            int g = ((Number)((n / 100.0))).intValue();
            factors = primeFactors(g);
            factors = java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(2)).toArray();
            factors = java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(2)).toArray();
            factors = java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(5)).toArray();
            factors = java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(5)).toArray();
        }
        int c = factors.length;
        if (c == 1) {
            return 1.0;
        }
        if (c == 2) {
            return ((Number)((factors[0] + factors[1]))).doubleValue();
        }
        double d = n / (factors[0]);
        return D(d) * (factors[0]) + d;
    }

    static String pad(int n) {
        String s = String.valueOf(n);
        while (s.length() < 4) {
            s = String.valueOf(" " + s);
        }
        return s;
    }

    static void main() {
        int[] vals = new int[]{};
        int n = -99;
        while (n < 101) {
            vals = java.util.stream.IntStream.concat(java.util.Arrays.stream(vals), java.util.stream.IntStream.of(((Number)((D(((Number)(n)).doubleValue())))).intValue())).toArray();
            n = n + 1;
        }
        int i = 0;
        while (i < vals.length) {
            String line = "";
            int j = 0;
            while (j < 10) {
                line = String.valueOf(line + String.valueOf(pad(vals[i + j])));
                if (j < 9) {
                    line = String.valueOf(line + " ");
                }
                j = j + 1;
            }
            System.out.println(line);
            i = i + 10;
        }
        double pow = 1.0;
        int m = 1;
        while (m < 21) {
            pow = pow * 10.0;
            String exp = String.valueOf(m);
            if (exp.length() < 2) {
                exp = String.valueOf(exp + " ");
            }
            String res = String.valueOf(String.valueOf(m) + String.valueOf(repeat("0", m - 1)));
            System.out.println(String.valueOf(String.valueOf("D(10^" + exp) + ") / 7 = ") + res);
            m = m + 1;
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
}
