public class Main {

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static java.util.Map<String,Double> electrical_impedance(double resistance, double reactance, double impedance) {
        int zero_count = 0;
        if (resistance == 0.0) {
            zero_count = zero_count + 1;
        }
        if (reactance == 0.0) {
            zero_count = zero_count + 1;
        }
        if (impedance == 0.0) {
            zero_count = zero_count + 1;
        }
        if (zero_count != 1) {
            throw new RuntimeException(String.valueOf("One and only one argument must be 0"));
        }
        if (resistance == 0.0) {
            double value = sqrtApprox(impedance * impedance - reactance * reactance);
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("resistance", value)));
        } else         if (reactance == 0.0) {
            double value_1 = sqrtApprox(impedance * impedance - resistance * resistance);
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("reactance", value_1)));
        } else         if (impedance == 0.0) {
            double value_2 = sqrtApprox(resistance * resistance + reactance * reactance);
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("impedance", value_2)));
        } else {
            throw new RuntimeException(String.valueOf("Exactly one argument must be 0"));
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(electrical_impedance(3.0, 4.0, 0.0));
            System.out.println(electrical_impedance(0.0, 4.0, 5.0));
            System.out.println(electrical_impedance(3.0, 0.0, 5.0));
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
