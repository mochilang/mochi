public class Main {
    static class LCG {
        java.math.BigInteger multiplier;
        java.math.BigInteger increment;
        java.math.BigInteger modulo;
        java.math.BigInteger seed;
        LCG(java.math.BigInteger multiplier, java.math.BigInteger increment, java.math.BigInteger modulo, java.math.BigInteger seed) {
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
    static java.math.BigInteger i = java.math.BigInteger.valueOf(0);

    static LCG make_lcg(java.math.BigInteger multiplier, java.math.BigInteger increment, java.math.BigInteger modulo, java.math.BigInteger seed) {
        return new LCG(multiplier, increment, modulo, seed);
    }

    static java.math.BigInteger next_number(LCG lcg) {
lcg.seed = (lcg.multiplier.multiply(lcg.seed).add(lcg.increment)).remainder(lcg.modulo);
        return lcg.seed;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            lcg = make_lcg(java.math.BigInteger.valueOf(1664525), java.math.BigInteger.valueOf(1013904223), java.math.BigInteger.valueOf(4294967296L), new java.math.BigInteger(String.valueOf(_now())));
            while (i.compareTo(java.math.BigInteger.valueOf(5)) < 0) {
                System.out.println(_p(next_number(lcg)));
                i = i.add(java.math.BigInteger.valueOf(1));
            }
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
