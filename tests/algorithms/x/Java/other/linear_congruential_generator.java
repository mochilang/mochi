public class Main {
    static class LCG {
        int multiplier;
        int increment;
        int modulo;
        int seed;
        LCG(int multiplier, int increment, int modulo, int seed) {
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
    static int i = 0;

    static LCG make_lcg(int multiplier, int increment, int modulo, int seed) {
        return new LCG(multiplier, increment, modulo, seed);
    }

    static int next_number(LCG lcg) {
lcg.seed = Math.floorMod((lcg.multiplier * lcg.seed + lcg.increment), lcg.modulo);
        return lcg.seed;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            lcg = make_lcg(1664525, 1013904223, (int)4294967296L, _now());
            i = 0;
            while (i < 5) {
                System.out.println(_p(next_number(lcg)));
                i = i + 1;
            }
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
