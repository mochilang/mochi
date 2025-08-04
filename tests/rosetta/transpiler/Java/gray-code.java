public class Main {

    static int xor(int a, int b) {
        int res = 0;
        int bit = 1;
        int x = a;
        int y = b;
        while (x > 0 || y > 0) {
            if ((Math.floorMod((Math.floorMod(x, 2) + Math.floorMod(y, 2)), 2) == 1)) {
                res = res + bit;
            }
            x = x / 2;
            y = y / 2;
            bit = bit * 2;
        }
        return res;
    }

    static int enc(int b) {
        return xor(b, b / 2);
    }

    static int dec(int g) {
        int b = 0;
        int x_1 = g;
        while (x_1 > 0) {
            b = xor(b, x_1);
            x_1 = x_1 / 2;
        }
        return b;
    }

    static String binary(int n) {
        if (n == 0) {
            return "0";
        }
        String s = "";
        int x_2 = n;
        while (x_2 > 0) {
            if (Math.floorMod(x_2, 2) == 1) {
                s = "1" + s;
            } else {
                s = "0" + s;
            }
            x_2 = x_2 / 2;
        }
        return s;
    }

    static String pad5(String s) {
        String p = s;
        while (_runeLen(p) < 5) {
            p = "0" + p;
        }
        return p;
    }

    static void main() {
        System.out.println("decimal  binary   gray    decoded");
        int b_1 = 0;
        while (b_1 < 32) {
            int g = enc(b_1);
            int d = dec(g);
            System.out.println("  " + String.valueOf(pad5(String.valueOf(binary(b_1)))) + "   " + String.valueOf(pad5(String.valueOf(binary(g)))) + "   " + String.valueOf(pad5(String.valueOf(binary(d)))) + "  " + _p(d));
            b_1 = b_1 + 1;
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
