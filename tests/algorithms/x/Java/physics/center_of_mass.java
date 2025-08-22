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
        double scaled = (double)((double)(x) * (double)(100.0));
        double rounded_1 = (double)(((Number)((((Number)(((double)(scaled) + (double)(0.5)))).intValue()))).doubleValue());
        return (double)(rounded_1) / (double)(100.0);
    }

    static Coord3D center_of_mass(Particle[] ps) {
        if ((long)(ps.length) == 0L) {
            throw new RuntimeException(String.valueOf("No particles provided"));
        }
        long i_1 = 0L;
        double total_mass_1 = (double)(0.0);
        while ((long)(i_1) < (long)(ps.length)) {
            Particle p_1 = ps[(int)((long)(i_1))];
            if ((double)(p_1.mass) <= (double)(0.0)) {
                throw new RuntimeException(String.valueOf("Mass of all particles must be greater than 0"));
            }
            total_mass_1 = (double)((double)(total_mass_1) + (double)(p_1.mass));
            i_1 = (long)((long)(i_1) + 1L);
        }
        double sum_x_1 = (double)(0.0);
        double sum_y_1 = (double)(0.0);
        double sum_z_1 = (double)(0.0);
        i_1 = 0L;
        while ((long)(i_1) < (long)(ps.length)) {
            Particle p_3 = ps[(int)((long)(i_1))];
            sum_x_1 = (double)((double)(sum_x_1) + (double)((double)(p_3.x) * (double)(p_3.mass)));
            sum_y_1 = (double)((double)(sum_y_1) + (double)((double)(p_3.y) * (double)(p_3.mass)));
            sum_z_1 = (double)((double)(sum_z_1) + (double)((double)(p_3.z) * (double)(p_3.mass)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        double cm_x_1 = (double)(round2((double)((double)(sum_x_1) / (double)(total_mass_1))));
        double cm_y_1 = (double)(round2((double)((double)(sum_y_1) / (double)(total_mass_1))));
        double cm_z_1 = (double)(round2((double)((double)(sum_z_1) / (double)(total_mass_1))));
        return new Coord3D(cm_x_1, cm_y_1, cm_z_1);
    }

    static String coord_to_string(Coord3D c) {
        return "Coord3D(x=" + _p(c.x) + ", y=" + _p(c.y) + ", z=" + _p(c.z) + ")";
    }
    public static void main(String[] args) {
        r1 = center_of_mass(((Particle[])(new Particle[]{new Particle(1.5, 4.0, 3.4, 4.0), new Particle(5.0, 6.8, 7.0, 8.1), new Particle(9.4, 10.1, 11.6, 12.0)})));
        System.out.println(coord_to_string(r1));
        r2 = center_of_mass(((Particle[])(new Particle[]{new Particle(1.0, 2.0, 3.0, 4.0), new Particle(5.0, 6.0, 7.0, 8.0), new Particle(9.0, 10.0, 11.0, 12.0)})));
        System.out.println(coord_to_string(r2));
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
