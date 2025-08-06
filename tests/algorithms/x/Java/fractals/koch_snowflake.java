public class Main {
    static class Vec {
        double x;
        double y;
        Vec(double x, double y) {
            this.x = x;
            this.y = y;
        }
        Vec() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static double PI;
    static double TWO_PI;
    static Vec VECTOR_1;
    static Vec VECTOR_2;
    static Vec VECTOR_3;
    static Vec[] INITIAL_VECTORS;
    static Vec[] example;

    static double _mod(double x, double m) {
        return x - (((Number)(((Number)(x / m)).intValue())).doubleValue()) * m;
    }

    static double sin(double x) {
        double y = _mod(x + PI, TWO_PI) - PI;
        double y2 = y * y;
        double y3 = y2 * y;
        double y5 = y3 * y2;
        double y7 = y5 * y2;
        return y - y3 / 6.0 + y5 / 120.0 - y7 / 5040.0;
    }

    static double cos(double x) {
        double y_1 = _mod(x + PI, TWO_PI) - PI;
        double y2_1 = y_1 * y_1;
        double y4 = y2_1 * y2_1;
        double y6 = y4 * y2_1;
        return 1.0 - y2_1 / 2.0 + y4 / 24.0 - y6 / 720.0;
    }

    static Vec rotate(Vec v, double angle_deg) {
        double theta = angle_deg * PI / 180.0;
        double c = cos(theta);
        double s = sin(theta);
        return new Vec(v.x * c - v.y * s, v.x * s + v.y * c);
    }

    static Vec[] iteration_step(Vec[] vectors) {
        Vec[] new_vectors = ((Vec[])(new Vec[]{}));
        int i = 0;
        while (i < vectors.length - 1) {
            Vec start = vectors[i];
            Vec end = vectors[i + 1];
            new_vectors = ((Vec[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_vectors), java.util.stream.Stream.of(start)).toArray(Vec[]::new)));
            double dx = end.x - start.x;
            double dy = end.y - start.y;
            Vec one_third = new Vec(start.x + dx / 3.0, start.y + dy / 3.0);
            Vec mid = rotate(new Vec(dx / 3.0, dy / 3.0), 60.0);
            Vec peak = new Vec(one_third.x + mid.x, one_third.y + mid.y);
            Vec two_third = new Vec(start.x + dx * 2.0 / 3.0, start.y + dy * 2.0 / 3.0);
            new_vectors = ((Vec[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_vectors), java.util.stream.Stream.of(one_third)).toArray(Vec[]::new)));
            new_vectors = ((Vec[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_vectors), java.util.stream.Stream.of(peak)).toArray(Vec[]::new)));
            new_vectors = ((Vec[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_vectors), java.util.stream.Stream.of(two_third)).toArray(Vec[]::new)));
            i = i + 1;
        }
        new_vectors = ((Vec[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_vectors), java.util.stream.Stream.of(vectors[vectors.length - 1])).toArray(Vec[]::new)));
        return new_vectors;
    }

    static Vec[] iterate(Vec[] initial, int steps) {
        Vec[] vectors = ((Vec[])(initial));
        int i_1 = 0;
        while (i_1 < steps) {
            vectors = ((Vec[])(iteration_step(((Vec[])(vectors)))));
            i_1 = i_1 + 1;
        }
        return vectors;
    }

    static String vec_to_string(Vec v) {
        return "(" + _p(v.x) + ", " + _p(v.y) + ")";
    }

    static String vec_list_to_string(Vec[] lst) {
        String res = "[";
        int i_2 = 0;
        while (i_2 < lst.length) {
            res = res + String.valueOf(vec_to_string(lst[i_2]));
            if (i_2 < lst.length - 1) {
                res = res + ", ";
            }
            i_2 = i_2 + 1;
        }
        res = res + "]";
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            TWO_PI = 6.283185307179586;
            VECTOR_1 = new Vec(0.0, 0.0);
            VECTOR_2 = new Vec(0.5, 0.8660254);
            VECTOR_3 = new Vec(1.0, 0.0);
            INITIAL_VECTORS = ((Vec[])(new Vec[]{VECTOR_1, VECTOR_2, VECTOR_3, VECTOR_1}));
            example = ((Vec[])(iterate(((Vec[])(new Vec[]{VECTOR_1, VECTOR_3})), 1)));
            System.out.println(vec_list_to_string(((Vec[])(example))));
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
        return String.valueOf(v);
    }
}
