public class Main {
    static int[] n = new int[]{3, 5, 7};
    static int[] a = new int[]{2, 3, 2};
    static int res = crt(a, n);

    static int[] egcd(int a, int b) {
        if (a == 0) {
            return new int[]{b, 0, 1};
        }
        int[] res = egcd(Math.floorMod(b, a), a);
        int g = res[0];
        int x1 = res[1];
        int y1 = res[2];
        return new int[]{g, y1 - (b / a) * x1, x1};
    }

    static int modInv(int a, int m) {
        int[] r = egcd(a, m);
        if (r[0] != 1) {
            return 0;
        }
        int x = r[1];
        if (x < 0) {
            return x + m;
        }
        return x;
    }

    static int crt(int[] a, int[] n) {
        int prod = 1;
        int i = 0;
        while (i < n.length) {
            prod = prod * n[i];
            i = i + 1;
        }
        int x = 0;
        i = 0;
        while (i < n.length) {
            int ni = n[i];
            int ai = a[i];
            int p = prod / ni;
            int inv = modInv(Math.floorMod(p, ni), ni);
            x = x + ai * inv * p;
            i = i + 1;
        }
        return Math.floorMod(x, prod);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(String.valueOf(res) + " <nil>");
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
