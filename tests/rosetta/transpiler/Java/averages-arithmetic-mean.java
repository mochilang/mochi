public class Main {

    static java.util.Map<String,Object> mean(double[] v) {
        if (v.length == 0) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("ok", false)));
        }
        double sum = 0.0;
        int i = 0;
        while (i < v.length) {
            sum = sum + v[i];
            i = i + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("ok", true), java.util.Map.entry("mean", sum / (((Number)(v.length)).doubleValue()))));
    }

    static void main() {
        double[][] sets = new double[][]{new double[]{}, new double[]{3.0, 1.0, 4.0, 1.0, 5.0, 9.0}, new double[]{100000000000000000000.0, 3.0, 1.0, 4.0, 1.0, 5.0, 9.0, -100000000000000000000.0}, new double[]{10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.11}, new double[]{10.0, 20.0, 30.0, 40.0, 50.0, -100.0, 4.7, -1100.0}};
        for (double[] v : sets) {
            System.out.println("Vector: " + String.valueOf(v));
            java.util.Map<String,Object> r = mean(v);
            if (((boolean)r.getOrDefault("ok", false))) {
                System.out.println(String.valueOf(String.valueOf("Mean of " + String.valueOf(v.length)) + " numbers is ") + String.valueOf(((double)r.getOrDefault("mean", 0.0))));
            } else {
                System.out.println("Mean undefined");
            }
            System.out.println("");
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
