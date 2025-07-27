public class Main {

    static int weekday(int y, int m, int d) {
        int yy = y;
        int mm = m;
        if (mm < 3) {
            mm = mm + 12;
            yy = yy - 1;
        }
        int k = Math.floorMod(yy, 100);
        int j = ((Number)((yy / 100))).intValue();
        int a = ((Number)(((13 * (mm + 1)) / 5))).intValue();
        int b = ((Number)((k / 4))).intValue();
        int c = ((Number)((j / 4))).intValue();
        return Math.floorMod((d + a + k + b + c + 5 * j), 7);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (int year = 2008; year < 2122; year++) {
                if (weekday(year, 12, 25) == 1) {
                    System.out.println("25 December " + String.valueOf(year) + " is Sunday");
                }
            }
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
