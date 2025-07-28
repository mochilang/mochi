public class Main {

    static String repeat(String ch, int n) {
        String s = "";
        int i = 0;
        while (i < n) {
            s = s + ch;
            i = i + 1;
        }
        return s;
    }

    static void cubLine(int n, int dx, int dy, String cde) {
        String line = String.valueOf(repeat(" ", n + 1)) + cde.substring(0, 1);
        int d = 9 * dx - 1;
        while (d > 0) {
            line = line + cde.substring(1, 2);
            d = d - 1;
        }
        line = line + cde.substring(0, 1);
        line = line + String.valueOf(repeat(" ", dy)) + cde.substring(2, _runeLen(cde));
        System.out.println(line);
    }

    static void cuboid(int dx, int dy, int dz) {
        System.out.println("cuboid " + String.valueOf(dx) + " " + String.valueOf(dy) + " " + String.valueOf(dz) + ":");
        cubLine(dy + 1, dx, 0, "+-");
        int i = 1;
        while (i <= dy) {
            cubLine(dy - i + 1, dx, i - 1, "/ |");
            i = i + 1;
        }
        cubLine(0, dx, dy, "+-|");
        int j = 4 * dz - dy - 2;
        while (j > 0) {
            cubLine(0, dx, dy, "| |");
            j = j - 1;
        }
        cubLine(0, dx, dy, "| +");
        i = 1;
        while (i <= dy) {
            cubLine(0, dx, dy - i, "| /");
            i = i + 1;
        }
        cubLine(0, dx, 0, "+-\n");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            cuboid(2, 3, 4);
            System.out.println("");
            cuboid(1, 1, 1);
            System.out.println("");
            cuboid(6, 2, 1);
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
}
