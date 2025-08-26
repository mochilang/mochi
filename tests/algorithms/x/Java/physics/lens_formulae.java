public class Main {

    static double focal_length_of_lens(double object_distance_from_lens, double image_distance_from_lens) {
        if ((double)(object_distance_from_lens) == (double)(0.0) || (double)(image_distance_from_lens) == (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter non zero values with respect to the sign convention."));
        }
        return (double)((double)(1.0) / (double)(((double)(((double)(1.0) / (double)(image_distance_from_lens))) - (double)(((double)(1.0) / (double)(object_distance_from_lens))))));
    }

    static double object_distance(double focal_length_of_lens, double image_distance_from_lens) {
        if ((double)(image_distance_from_lens) == (double)(0.0) || (double)(focal_length_of_lens) == (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter non zero values with respect to the sign convention."));
        }
        return (double)((double)(1.0) / (double)(((double)(((double)(1.0) / (double)(image_distance_from_lens))) - (double)(((double)(1.0) / (double)(focal_length_of_lens))))));
    }

    static double image_distance(double focal_length_of_lens, double object_distance_from_lens) {
        if ((double)(object_distance_from_lens) == (double)(0.0) || (double)(focal_length_of_lens) == (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter non zero values with respect to the sign convention."));
        }
        return (double)((double)(1.0) / (double)(((double)(((double)(1.0) / (double)(object_distance_from_lens))) + (double)(((double)(1.0) / (double)(focal_length_of_lens))))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(focal_length_of_lens((double)(10.0), (double)(4.0))));
            System.out.println(_p(focal_length_of_lens((double)(2.7), (double)(5.8))));
            System.out.println(_p(object_distance((double)(10.0), (double)(40.0))));
            System.out.println(_p(object_distance((double)(6.2), (double)(1.5))));
            System.out.println(_p(image_distance((double)(50.0), (double)(40.0))));
            System.out.println(_p(image_distance((double)(5.3), (double)(7.9))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
