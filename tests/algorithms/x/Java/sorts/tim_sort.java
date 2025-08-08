public class Main {
    static int[] sample;
    static int[] sorted_sample;
    static int[] sample2;
    static int[] sorted_sample2;

    static int[] copy_list(int[] xs) {
        int[] res = ((int[])(new int[]{}));
        int k = 0;
        while (k < xs.length) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[k])).toArray()));
            k = k + 1;
        }
        return res;
    }

    static int[] insertion_sort(int[] xs) {
        int[] arr = ((int[])(copy_list(((int[])(xs)))));
        int idx = 1;
        while (idx < arr.length) {
            int value = arr[idx];
            int jdx = idx - 1;
            while (jdx >= 0 && arr[jdx] > value) {
arr[jdx + 1] = arr[jdx];
                jdx = jdx - 1;
            }
arr[jdx + 1] = value;
            idx = idx + 1;
        }
        return arr;
    }

    static int[] merge(int[] left, int[] right) {
        int[] result = ((int[])(new int[]{}));
        int i = 0;
        int j = 0;
        while (i < left.length && j < right.length) {
            if (left[i] < right[j]) {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(left[i])).toArray()));
                i = i + 1;
            } else {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(right[j])).toArray()));
                j = j + 1;
            }
        }
        while (i < left.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(left[i])).toArray()));
            i = i + 1;
        }
        while (j < right.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(right[j])).toArray()));
            j = j + 1;
        }
        return result;
    }

    static int[] tim_sort(int[] xs) {
        int n = xs.length;
        int[][] runs = ((int[][])(new int[][]{}));
        int[][] sorted_runs = ((int[][])(new int[][]{}));
        int[] current = ((int[])(new int[]{}));
        current = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(current), java.util.stream.IntStream.of(xs[0])).toArray()));
        int i_1 = 1;
        while (i_1 < n) {
            if (xs[i_1] < xs[i_1 - 1]) {
                runs = ((int[][])(appendObj(runs, copy_list(((int[])(current))))));
                current = ((int[])(new int[]{}));
                current = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(current), java.util.stream.IntStream.of(xs[i_1])).toArray()));
            } else {
                current = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(current), java.util.stream.IntStream.of(xs[i_1])).toArray()));
            }
            i_1 = i_1 + 1;
        }
        runs = ((int[][])(appendObj(runs, copy_list(((int[])(current))))));
        int r = 0;
        while (r < runs.length) {
            sorted_runs = ((int[][])(appendObj(sorted_runs, insertion_sort(((int[])(runs[r]))))));
            r = r + 1;
        }
        int[] result_1 = ((int[])(new int[]{}));
        r = 0;
        while (r < sorted_runs.length) {
            result_1 = ((int[])(merge(((int[])(result_1)), ((int[])(sorted_runs[r])))));
            r = r + 1;
        }
        return result_1;
    }

    static String list_to_string(int[] xs) {
        String s = "[";
        int k_1 = 0;
        while (k_1 < xs.length) {
            s = s + _p(_geti(xs, k_1));
            if (k_1 < xs.length - 1) {
                s = s + ", ";
            }
            k_1 = k_1 + 1;
        }
        return s + "]";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            sample = ((int[])(new int[]{5, 9, 10, 3, -4, 5, 178, 92, 46, -18, 0, 7}));
            sorted_sample = ((int[])(tim_sort(((int[])(sample)))));
            System.out.println(list_to_string(((int[])(sorted_sample))));
            sample2 = ((int[])(new int[]{3, 2, 1}));
            sorted_sample2 = ((int[])(tim_sort(((int[])(sample2)))));
            System.out.println(list_to_string(((int[])(sorted_sample2))));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
