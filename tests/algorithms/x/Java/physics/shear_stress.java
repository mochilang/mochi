public class Main {
    static class Result {
        String name;
        double value;
        Result(String name, double value) {
            this.name = name;
            this.value = value;
        }
        Result() {}
        @Override public String toString() {
            return String.format("{'name': '%s', 'value': %s}", String.valueOf(name), String.valueOf(value));
        }
    }

    static Result r1;
    static Result r2;
    static Result r3;

    static Result shear_stress(double stress, double tangential_force, double area) {
        long zeros = 0L;
        if ((double)(stress) == (double)(0.0)) {
            zeros = (long)((long)(zeros) + 1L);
        }
        if ((double)(tangential_force) == (double)(0.0)) {
            zeros = (long)((long)(zeros) + 1L);
        }
        if ((double)(area) == (double)(0.0)) {
            zeros = (long)((long)(zeros) + 1L);
        }
        if ((long)(zeros) != 1L) {
            throw new RuntimeException(String.valueOf("You cannot supply more or less than 2 values"));
        } else         if ((double)(stress) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Stress cannot be negative"));
        } else         if ((double)(tangential_force) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Tangential Force cannot be negative"));
        } else         if ((double)(area) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Area cannot be negative"));
        } else         if ((double)(stress) == (double)(0.0)) {
            return new Result("stress", (double)(tangential_force) / (double)(area));
        } else         if ((double)(tangential_force) == (double)(0.0)) {
            return new Result("tangential_force", (double)(stress) * (double)(area));
        } else {
            return new Result("area", (double)(tangential_force) / (double)(stress));
        }
    }

    static String str_result(Result r) {
        return "Result(name='" + r.name + "', value=" + _p(r.value) + ")";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            r1 = shear_stress((double)(25.0), (double)(100.0), (double)(0.0));
            System.out.println(str_result(r1));
            r2 = shear_stress((double)(0.0), (double)(1600.0), (double)(200.0));
            System.out.println(str_result(r2));
            r3 = shear_stress((double)(1000.0), (double)(0.0), (double)(1200.0));
            System.out.println(str_result(r3));
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
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
