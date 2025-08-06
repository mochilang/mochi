public class Main {

    static java.util.Map<String,Double> ohms_law(double voltage, double current, double resistance) {
        int zeros = 0;
        if (voltage == 0.0) {
            zeros = zeros + 1;
        }
        if (current == 0.0) {
            zeros = zeros + 1;
        }
        if (resistance == 0.0) {
            zeros = zeros + 1;
        }
        if (zeros != 1) {
            System.out.println("One and only one argument must be 0");
            return new java.util.LinkedHashMap<String, Double>();
        }
        if (resistance < 0.0) {
            System.out.println("Resistance cannot be negative");
            return new java.util.LinkedHashMap<String, Double>();
        }
        if (voltage == 0.0) {
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("voltage", current * resistance)));
        }
        if (current == 0.0) {
            return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("current", voltage / resistance)));
        }
        return new java.util.LinkedHashMap<String, Double>(java.util.Map.ofEntries(java.util.Map.entry("resistance", voltage / current)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            json(ohms_law(10.0, 0.0, 5.0));
            json(ohms_law(-10.0, 1.0, 0.0));
            json(ohms_law(0.0, -1.5, 2.0));
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

    static void json(Object v) {
        System.out.println(_json(v));
    }

    static String _json(Object v) {
        if (v == null) return "null";
        if (v instanceof String) {
            String s = (String)v;
            s = s.replace("\\", "\\\\").replace("\"", "\\\"");
            return "\"" + s + "\"";
        }
        if (v instanceof Number || v instanceof Boolean) {
            return String.valueOf(v);
        }
        if (v instanceof int[]) {
            int[] a = (int[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof double[]) {
            double[] a = (double[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof boolean[]) {
            boolean[] a = (boolean[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v.getClass().isArray()) {
            Object[] a = (Object[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(_json(a[i])); }
            sb.append("]");
            return sb.toString();
        }
        String s = String.valueOf(v);
        s = s.replace("\\", "\\\\").replace("\"", "\\\"");
        return "\"" + s + "\"";
    }
}
