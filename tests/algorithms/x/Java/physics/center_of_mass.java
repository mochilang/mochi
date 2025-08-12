public class Main {
    static class Particle {
        double x;
        double y;
        double z;
        double mass;
        Particle(double x, double y, double z, double mass) {
            this.x = x;
            this.y = y;
            this.z = z;
            this.mass = mass;
        }
        Particle() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'z': %s, 'mass': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(z), String.valueOf(mass));
        }
    }

    static class Coord3D {
        double x;
        double y;
        double z;
        Coord3D(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
        Coord3D() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'z': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(z));
        }
    }

    static Coord3D r1;
    static Coord3D r2;

    static double round2(double x) {
        double scaled = (double)(x) * 100.0;
        double rounded_1 = ((Number)((((Number)((scaled + 0.5))).intValue()))).doubleValue();
        return rounded_1 / 100.0;
    }

    static Coord3D center_of_mass(Particle[] ps) {
        if ((long)(ps.length) == (long)(0)) {
            throw new RuntimeException(String.valueOf("No particles provided"));
        }
        long i_1 = 0L;
        double total_mass_1 = 0.0;
        while (i_1 < (long)(ps.length)) {
            Particle p_1 = ps[(int)((long)(i_1))];
            if (p_1.mass <= 0.0) {
                throw new RuntimeException(String.valueOf("Mass of all particles must be greater than 0"));
            }
            total_mass_1 = (double)(total_mass_1) + p_1.mass;
            i_1 = (long)(i_1 + (long)(1));
        }
        double sum_x_1 = 0.0;
        double sum_y_1 = 0.0;
        double sum_z_1 = 0.0;
        i_1 = 0L;
        while (i_1 < (long)(ps.length)) {
            Particle p_3 = ps[(int)((long)(i_1))];
            sum_x_1 = (double)(sum_x_1) + p_3.x * p_3.mass;
            sum_y_1 = (double)(sum_y_1) + p_3.y * p_3.mass;
            sum_z_1 = (double)(sum_z_1) + p_3.z * p_3.mass;
            i_1 = (long)(i_1 + (long)(1));
        }
        double cm_x_1 = (double)(round2((double)(sum_x_1) / (double)(total_mass_1)));
        double cm_y_1 = (double)(round2((double)(sum_y_1) / (double)(total_mass_1)));
        double cm_z_1 = (double)(round2((double)(sum_z_1) / (double)(total_mass_1)));
        return new Coord3D(cm_x_1, cm_y_1, cm_z_1);
    }

    static String coord_to_string(Coord3D c) {
        return "Coord3D(x=" + _p(c.x) + ", y=" + _p(c.y) + ", z=" + _p(c.z) + ")";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            r1 = center_of_mass(((Particle[])(new Particle[]{new Particle(1.5, 4.0, 3.4, 4.0), new Particle(5.0, 6.8, 7.0, 8.1), new Particle(9.4, 10.1, 11.6, 12.0)})));
            System.out.println(coord_to_string(r1));
            r2 = center_of_mass(((Particle[])(new Particle[]{new Particle(1.0, 2.0, 3.0, 4.0), new Particle(5.0, 6.0, 7.0, 8.0), new Particle(9.0, 10.0, 11.0, 12.0)})));
            System.out.println(coord_to_string(r2));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
