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
        return (double)((double)(rounded_1) / (double)(100.0));
    }

    static Coord3D center_of_mass(Particle[] ps) {
        if (new java.math.BigInteger(String.valueOf(ps.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            throw new RuntimeException(String.valueOf("No particles provided"));
        }
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        double total_mass_1 = (double)(0.0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(ps.length))) < 0) {
            Particle p_1 = ps[_idx((ps).length, ((java.math.BigInteger)(i_1)).longValue())];
            if ((double)(p_1.mass) <= (double)(0.0)) {
                throw new RuntimeException(String.valueOf("Mass of all particles must be greater than 0"));
            }
            total_mass_1 = (double)((double)(total_mass_1) + (double)(p_1.mass));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        double sum_x_1 = (double)(0.0);
        double sum_y_1 = (double)(0.0);
        double sum_z_1 = (double)(0.0);
        i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(ps.length))) < 0) {
            Particle p_3 = ps[_idx((ps).length, ((java.math.BigInteger)(i_1)).longValue())];
            sum_x_1 = (double)((double)(sum_x_1) + (double)((double)(p_3.x) * (double)(p_3.mass)));
            sum_y_1 = (double)((double)(sum_y_1) + (double)((double)(p_3.y) * (double)(p_3.mass)));
            sum_z_1 = (double)((double)(sum_z_1) + (double)((double)(p_3.z) * (double)(p_3.mass)));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        double cm_x_1 = (double)(round2((double)((double)(sum_x_1) / (double)(total_mass_1))));
        double cm_y_1 = (double)(round2((double)((double)(sum_y_1) / (double)(total_mass_1))));
        double cm_z_1 = (double)(round2((double)((double)(sum_z_1) / (double)(total_mass_1))));
        return new Coord3D((double)(cm_x_1), (double)(cm_y_1), (double)(cm_z_1));
    }

    static String coord_to_string(Coord3D c) {
        return "Coord3D(x=" + _p(c.x) + ", y=" + _p(c.y) + ", z=" + _p(c.z) + ")";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            r1 = center_of_mass(((Particle[])(new Particle[]{new Particle((double)(1.5), (double)(4.0), (double)(3.4), (double)(4.0)), new Particle((double)(5.0), (double)(6.8), (double)(7.0), (double)(8.1)), new Particle((double)(9.4), (double)(10.1), (double)(11.6), (double)(12.0))})));
            System.out.println(coord_to_string(r1));
            r2 = center_of_mass(((Particle[])(new Particle[]{new Particle((double)(1.0), (double)(2.0), (double)(3.0), (double)(4.0)), new Particle((double)(5.0), (double)(6.0), (double)(7.0), (double)(8.0)), new Particle((double)(9.0), (double)(10.0), (double)(11.0), (double)(12.0))})));
            System.out.println(coord_to_string(r2));
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
