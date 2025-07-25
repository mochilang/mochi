public class Main {
    static double[][] testCases = new double[][]{new double[]{20.0, 45.0}, new double[]{0 - 45.0, 45.0}, new double[]{0 - 85.0, 90.0}, new double[]{0 - 95.0, 90.0}, new double[]{0 - 45.0, 125.0}, new double[]{0 - 45.0, 145.0}, new double[]{29.4803, 0 - 88.6381}, new double[]{0 - 78.3251, 0 - 159.036}, new double[]{0 - 70099.74233810938, 29840.67437876723}, new double[]{0 - 165313.6666297357, 33693.9894517456}, new double[]{1174.8380510598456, 0 - 154146.66490124757}, new double[]{60175.77306795546, 42213.07192354373}};

    static double angleDiff(double b1, double b2) {
        double diff = b2 - b1;
        return ((diff % 360.0 + 360.0 + 180.0) % 360.0) - 180.0;
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
