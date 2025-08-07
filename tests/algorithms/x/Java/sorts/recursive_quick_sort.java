public class Main {

    static int[] concat(int[] a, int[] b) {
        int[] result = ((int[])(new int[]{}));
        for (int x : a) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(x)).toArray()));
        }
        for (int x : b) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(x)).toArray()));
        }
        return result;
    }

    static int[] quick_sort(int[] data) {
        if (data.length <= 1) {
            return data;
        }
        int pivot = data[0];
        int[] left = ((int[])(new int[]{}));
        int[] right = ((int[])(new int[]{}));
        int i = 1;
        while (i < data.length) {
            int e = data[i];
            if (e <= pivot) {
                left = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(left), java.util.stream.IntStream.of(e)).toArray()));
            } else {
                right = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(right), java.util.stream.IntStream.of(e)).toArray()));
            }
            i = i + 1;
        }
        int[] sorted_left = ((int[])(quick_sort(((int[])(left)))));
        int[] sorted_right = ((int[])(quick_sort(((int[])(right)))));
        int[] left_pivot = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(sorted_left), java.util.stream.IntStream.of(pivot)).toArray()));
        return concat(((int[])(left_pivot)), ((int[])(sorted_right)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(quick_sort(((int[])(new int[]{2, 1, 0})))));
            System.out.println(_p(quick_sort(((int[])(new int[]{3, 5, 2, 4, 1})))));
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

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
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
