public class Main {
    static class FromTo {
        double from_factor;
        double to_factor;
        FromTo(double from_factor, double to_factor) {
            this.from_factor = from_factor;
            this.to_factor = to_factor;
        }
        FromTo() {}
        @Override public String toString() {
            return String.format("{'from_factor': %s, 'to_factor': %s}", String.valueOf(from_factor), String.valueOf(to_factor));
        }
    }

    static java.util.Map<String,FromTo> PRESSURE_CONVERSION;

    static double pressure_conversion(double value, String from_type, String to_type) {
        if (!(PRESSURE_CONVERSION.containsKey(from_type))) {
            Object keys = String.join(", ", PRESSURE_CONVERSION.keySet());
            throw new RuntimeException(String.valueOf("Invalid 'from_type' value: '" + from_type + "'  Supported values are:\n" + (String)(keys)));
        }
        if (!(PRESSURE_CONVERSION.containsKey(to_type))) {
            Object keys_1 = String.join(", ", PRESSURE_CONVERSION.keySet());
            throw new RuntimeException(String.valueOf("Invalid 'to_type' value: '" + to_type + ".  Supported values are:\n" + (String)(keys_1)));
        }
        FromTo from = (FromTo)(((FromTo)(PRESSURE_CONVERSION).get(from_type)));
        FromTo to = (FromTo)(((FromTo)(PRESSURE_CONVERSION).get(to_type)));
        return value * from.from_factor * to.to_factor;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PRESSURE_CONVERSION = ((java.util.Map<String,FromTo>)(new java.util.LinkedHashMap<String, FromTo>(java.util.Map.ofEntries(java.util.Map.entry("atm", new FromTo(1.0, 1.0)), java.util.Map.entry("pascal", new FromTo(9.8e-06, 101325.0)), java.util.Map.entry("bar", new FromTo(0.986923, 1.01325)), java.util.Map.entry("kilopascal", new FromTo(0.00986923, 101.325)), java.util.Map.entry("megapascal", new FromTo(9.86923, 0.101325)), java.util.Map.entry("psi", new FromTo(0.068046, 14.6959)), java.util.Map.entry("inHg", new FromTo(0.0334211, 29.9213)), java.util.Map.entry("torr", new FromTo(0.00131579, 760.0))))));
            System.out.println(pressure_conversion(4.0, "atm", "pascal"));
            System.out.println(pressure_conversion(1.0, "pascal", "psi"));
            System.out.println(pressure_conversion(1.0, "bar", "atm"));
            System.out.println(pressure_conversion(3.0, "kilopascal", "bar"));
            System.out.println(pressure_conversion(2.0, "megapascal", "psi"));
            System.out.println(pressure_conversion(4.0, "psi", "torr"));
            System.out.println(pressure_conversion(1.0, "inHg", "atm"));
            System.out.println(pressure_conversion(1.0, "torr", "psi"));
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
