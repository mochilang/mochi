public class Main {
    static double[][] testCases = new double[][]{new double[]{20.0, 45.0}, new double[]{0 - 45.0, 45.0}, new double[]{0 - 85.0, 90.0}, new double[]{0 - 95.0, 90.0}, new double[]{0 - 45.0, 125.0}, new double[]{0 - 45.0, 145.0}, new double[]{29.4803, 0 - 88.6381}, new double[]{0 - 78.3251, 0 - 159.036}};

    static double angleDiff(double b1, double b2) {
        double d = b2 - b1;
        if (d < 0 - 180.0) {
            return d + 360.0;
        }
        if (d > 180.0) {
            return d - 360.0;
        }
        return d;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (var tc : testCases) {
                System.out.println(angleDiff(tc[0], tc[1]));
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
