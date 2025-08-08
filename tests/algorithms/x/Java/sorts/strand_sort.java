public class Main {

    static int[] merge(int[] xs, int[] ys, boolean reverse) {
        int[] result = ((int[])(new int[]{}));
        int i = 0;
        int j = 0;
        while (i < xs.length && j < ys.length) {
            if (((Boolean)(reverse))) {
                if (xs[i] > ys[j]) {
                    result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(xs[i])).toArray()));
                    i = i + 1;
                } else {
                    result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(ys[j])).toArray()));
                    j = j + 1;
                }
            } else             if (xs[i] < ys[j]) {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(xs[i])).toArray()));
                i = i + 1;
            } else {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(ys[j])).toArray()));
                j = j + 1;
            }
        }
        while (i < xs.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(xs[i])).toArray()));
            i = i + 1;
        }
        while (j < ys.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(ys[j])).toArray()));
            j = j + 1;
        }
        return result;
    }

    static int[] strand_sort_rec(int[] arr, boolean reverse, int[] solution) {
        if (arr.length == 0) {
            return solution;
        }
        int[] sublist = ((int[])(new int[]{}));
        int[] remaining = ((int[])(new int[]{}));
        sublist = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(sublist), java.util.stream.IntStream.of(arr[0])).toArray()));
        int last = arr[0];
        int k = 1;
        while (k < arr.length) {
            int item = arr[k];
            if (((Boolean)(reverse))) {
                if (item < last) {
                    sublist = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(sublist), java.util.stream.IntStream.of(item)).toArray()));
                    last = item;
                } else {
                    remaining = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(remaining), java.util.stream.IntStream.of(item)).toArray()));
                }
            } else             if (item > last) {
                sublist = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(sublist), java.util.stream.IntStream.of(item)).toArray()));
                last = item;
            } else {
                remaining = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(remaining), java.util.stream.IntStream.of(item)).toArray()));
            }
            k = k + 1;
        }
        solution = ((int[])(merge(((int[])(solution)), ((int[])(sublist)), reverse)));
        return strand_sort_rec(((int[])(remaining)), reverse, ((int[])(solution)));
    }

    static int[] strand_sort(int[] arr, boolean reverse) {
        return strand_sort_rec(((int[])(arr)), reverse, ((int[])(new int[]{})));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(strand_sort(((int[])(new int[]{4, 3, 5, 1, 2})), false)));
            System.out.println(_p(strand_sort(((int[])(new int[]{4, 3, 5, 1, 2})), true)));
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
