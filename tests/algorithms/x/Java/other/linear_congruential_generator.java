public class Main {
    static class LCG {
        long multiplier;
        long increment;
        long modulo;
        long seed;
        LCG(long multiplier, long increment, long modulo, long seed) {
            this.multiplier = multiplier;
            this.increment = increment;
            this.modulo = modulo;
            this.seed = seed;
        }
        LCG() {}
        @Override public String toString() {
            return String.format("{'multiplier': %s, 'increment': %s, 'modulo': %s, 'seed': %s}", String.valueOf(multiplier), String.valueOf(increment), String.valueOf(modulo), String.valueOf(seed));
        }
    }

    static LCG lcg = null;
    static long i = 0L;

    static LCG make_lcg(long multiplier, long increment, long modulo, long seed) {
        return new LCG(multiplier, increment, modulo, seed);
    }

    static long next_number(LCG lcg) {
lcg.seed = Math.floorMod(((long)((long)(lcg.multiplier) * (long)(lcg.seed)) + (long)(lcg.increment)), lcg.modulo);
        return lcg.seed;
    }
    public static void main(String[] args) {
        lcg = make_lcg(1664525L, 1013904223L, 4294967296L, (long)(_now()));
        while ((long)(i) < 5L) {
            System.out.println(_p(next_number(lcg)));
            i = (long)((long)(i) + 1L);
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
