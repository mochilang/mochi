public class Main {
    static double PI;

    static java.util.Map<String,Double> ind_reactance(double inductance, double frequency, double reactance) {
        int zero_count = 0;
        if (inductance == 0.0) {
            zero_count = zero_count + 1;
        }
        if (frequency == 0.0) {
            zero_count = zero_count + 1;
        }
        if (reactance == 0.0) {
            zero_count = zero_count + 1;
        }
        if (zero_count != 1) {
            throw new RuntimeException(String.valueOf("One and only one argument must be 0"));
        }
        if (inductance < 0.0) {
            throw new RuntimeException(String.valueOf("Inductance cannot be negative"));
        }
        if (frequency < 0.0) {
            throw new RuntimeException(String.valueOf("Frequency cannot be negative"));
        }
        if (reactance < 0.0) {
            throw new RuntimeException(String.valueOf("Inductive reactance cannot be negative"));
        }
        if (inductance == 0.0) {
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("inductance", reactance / (2.0 * PI * frequency))));
        }
        if (frequency == 0.0) {
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("frequency", reactance / (2.0 * PI * inductance))));
        }
        return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("reactance", 2.0 * PI * frequency * inductance)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            System.out.println(ind_reactance(0.0, 10000.0, 50.0));
            System.out.println(ind_reactance(0.035, 0.0, 50.0));
            System.out.println(ind_reactance(3.5e-05, 1000.0, 0.0));
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
