public class Main {
    static int b;
    static int r;
    static String s;

    static int ord(String ch) {
        if ((ch.equals("a"))) {
            return 97;
        }
        if ((ch.equals("π"))) {
            return 960;
        }
        if ((ch.equals("A"))) {
            return 65;
        }
        return 0;
    }

    static String chr(int n) {
        if (n == 97) {
            return "a";
        }
        if (n == 960) {
            return "π";
        }
        if (n == 65) {
            return "A";
        }
        return "?";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            b = ord("a");
            r = ord("π");
            s = "aπ";
            System.out.println(_p(b) + " " + _p(r) + " " + s);
            System.out.println("string cast to []rune: [" + _p(b) + " " + _p(r) + "]");
            System.out.println("    string range loop: " + _p(b) + " " + _p(r));
            System.out.println("         string bytes: 0x61 0xcf 0x80");
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
