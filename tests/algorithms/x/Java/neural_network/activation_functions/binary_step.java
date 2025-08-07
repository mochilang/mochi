public class Main {

    static int[] binary_step(double[] vector) {
        int[] out = ((int[])(new int[]{}));
        int i = 0;
        while (i < vector.length) {
            if (vector[i] >= 0.0) {
                out = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(1)).toArray()));
            } else {
                out = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(0)).toArray()));
            }
            i = i + 1;
        }
        return out;
    }

    static void main() {
        double[] vector = ((double[])(new double[]{-1.2, 0.0, 2.0, 1.45, -3.7, 0.3}));
        int[] result = ((int[])(binary_step(((double[])(vector)))));
        System.out.println(java.util.Arrays.toString(result));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
}
