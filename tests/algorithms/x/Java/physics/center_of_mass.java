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
        double scaled = x * 100.0;
        double rounded = ((Number)((((Number)((scaled + 0.5))).intValue()))).doubleValue();
        return rounded / 100.0;
    }

    static Coord3D center_of_mass(Particle[] ps) {
        if (ps.length == 0) {
            throw new RuntimeException(String.valueOf("No particles provided"));
        }
        int i = 0;
        double total_mass = 0.0;
        while (i < ps.length) {
            Particle p = ps[i];
            if (p.mass <= 0.0) {
                throw new RuntimeException(String.valueOf("Mass of all particles must be greater than 0"));
            }
            total_mass = total_mass + p.mass;
            i = i + 1;
        }
        double sum_x = 0.0;
        double sum_y = 0.0;
        double sum_z = 0.0;
        i = 0;
        while (i < ps.length) {
            Particle p_1 = ps[i];
            sum_x = sum_x + p_1.x * p_1.mass;
            sum_y = sum_y + p_1.y * p_1.mass;
            sum_z = sum_z + p_1.z * p_1.mass;
            i = i + 1;
        }
        double cm_x = round2(sum_x / total_mass);
        double cm_y = round2(sum_y / total_mass);
        double cm_z = round2(sum_z / total_mass);
        return new Coord3D(cm_x, cm_y, cm_z);
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
        return String.valueOf(v);
    }
}
