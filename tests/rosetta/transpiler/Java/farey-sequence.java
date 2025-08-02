public class Main {
    static class Frac {
        int num;
        int den;
        Frac(int num, int den) {
            this.num = num;
            this.den = den;
        }
        @Override public String toString() {
            return String.format("{'num': %s, 'den': %s}", String.valueOf(num), String.valueOf(den));
        }
    }


    static String fracStr(Frac f) {
        return (String)(_p(f.num)) + "/" + (String)(_p(f.den));
    }

    static Frac[] gen(Frac l, Frac r, int n, Frac[] acc) {
        Frac m = new Frac(l.num + r.num, l.den + r.den);
        if (m.den <= n) {
            acc = gen(l, m, n, acc);
            acc = java.util.stream.Stream.concat(java.util.Arrays.stream(acc), java.util.stream.Stream.of(m)).toArray(Frac[]::new);
            acc = gen(m, r, n, acc);
        }
        return acc;
    }

    static int totient(int n) {
        int tot = n;
        int nn = n;
        int p = 2;
        while (p * p <= nn) {
            if (Math.floorMod(nn, p) == 0) {
                while (Math.floorMod(nn, p) == 0) {
                    nn = nn / p;
                }
                tot = tot - tot / p;
            }
            if (p == 2) {
                p = 1;
            }
            p = p + 2;
        }
        if (nn > 1) {
            tot = tot - tot / nn;
        }
        return tot;
    }

    static void main() {
        int n = 1;
        while (n <= 11) {
            Frac l = new Frac(0, 1);
            Frac r = new Frac(1, 1);
            Frac[] seq = gen(l, r, n, new Frac[]{});
            String line = "F(" + (String)(_p(n)) + "): " + String.valueOf(fracStr(l));
            for (Frac f : seq) {
                line = line + " " + String.valueOf(fracStr(f));
            }
            line = line + " " + String.valueOf(fracStr(r));
            System.out.println(line);
            n = n + 1;
        }
        int sum = 1;
        int i = 1;
        int next = 100;
        while (i <= 1000) {
            sum = sum + totient(i);
            if (i == next) {
                System.out.println("|F(" + (String)(_p(i)) + ")|: " + (String)(_p(sum)));
                next = next + 100;
            }
            i = i + 1;
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
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
