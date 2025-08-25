public class Main {

    static java.math.BigInteger[] binary_step(double[] vector) {
        java.math.BigInteger[] out = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(vector.length))) < 0) {
            if ((double)(vector[_idx((vector).length, ((java.math.BigInteger)(i_1)).longValue())]) >= (double)(0.0)) {
                out = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(java.math.BigInteger.valueOf(1))).toArray(java.math.BigInteger[]::new)));
            } else {
                out = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[])(out));
    }

    static void main() {
        double[] vector = ((double[])(new double[]{(double)(-1.2), (double)(0.0), (double)(2.0), (double)(1.45), (double)(-3.7), (double)(0.3)}));
        java.math.BigInteger[] result_1 = ((java.math.BigInteger[])(binary_step(((double[])(vector)))));
        System.out.println(java.util.Arrays.toString(result_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
