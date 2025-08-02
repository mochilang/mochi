public class Main {

    static java.math.BigInteger parseBigInt(String str) {
        int i = 0;
        boolean neg = false;
        if (_runeLen(str) > 0 && (_substr(str, 0, 1).equals("-"))) {
            neg = true;
            i = 1;
        }
        java.math.BigInteger n = java.math.BigInteger.valueOf(0);
        while (i < _runeLen(str)) {
            String ch = _substr(str, i, i + 1);
            int d = Integer.parseInt(ch);
            n = n.multiply((java.math.BigInteger.valueOf(10))).add((new java.math.BigInteger(String.valueOf(d))));
            i = i + 1;
        }
        if (neg) {
            n = (n).negate();
        }
        return n;
    }

    static String pad(int n, int width) {
        String s = (String)(_p(n));
        while (_runeLen(s) < width) {
            s = " " + s;
        }
        return s;
    }

    static void showInt(int n) {
        String line = "Testing integer " + String.valueOf(pad(n, 3)) + ":  ";
        if (Math.floorMod(n, 2) == 0) {
            line = line + "even ";
        } else {
            line = line + " odd ";
        }
        if (Math.floorMod(n, 2) == 0) {
            line = line + "even";
        } else {
            line = line + " odd";
        }
        System.out.println(line);
    }

    static void showBig(String s) {
        java.math.BigInteger b = parseBigInt(s);
        String line_1 = "Testing big integer " + (String)(_p(b)) + ":  ";
        if (b.remainder((java.math.BigInteger.valueOf(2))).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            line_1 = line_1 + "even";
        } else {
            line_1 = line_1 + "odd";
        }
        System.out.println(line_1);
    }

    static void main() {
        showInt(-2);
        showInt(-1);
        showInt(0);
        showInt(1);
        showInt(2);
        showBig("-222222222222222222222222222222222222");
        showBig("-1");
        showBig("0");
        showBig("1");
        showBig("222222222222222222222222222222222222");
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
