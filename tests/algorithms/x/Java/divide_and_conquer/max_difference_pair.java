public class Main {

    static int min_slice(int[] a, int start, int end) {
        int m = a[start];
        int i = start + 1;
        while (i < end) {
            if (a[i] < m) {
                m = a[i];
            }
            i = i + 1;
        }
        return m;
    }

    static int max_slice(int[] a, int start, int end) {
        int m_1 = a[start];
        int i_1 = start + 1;
        while (i_1 < end) {
            if (a[i_1] > m_1) {
                m_1 = a[i_1];
            }
            i_1 = i_1 + 1;
        }
        return m_1;
    }

    static int[] max_diff_range(int[] a, int start, int end) {
        if (end - start == 1) {
            int v = a[start];
            return new int[]{v, v};
        }
        int mid = (start + end) / 2;
        int[] left = ((int[])(max_diff_range(((int[])(a)), start, mid)));
        int[] right = ((int[])(max_diff_range(((int[])(a)), mid, end)));
        int small1 = left[0];
        int big1 = left[1];
        int small2 = right[0];
        int big2 = right[1];
        int min_left = min_slice(((int[])(a)), start, mid);
        int max_right = max_slice(((int[])(a)), mid, end);
        int cross_diff = max_right - min_left;
        int left_diff = big1 - small1;
        int right_diff = big2 - small2;
        if (right_diff > cross_diff && right_diff > left_diff) {
            return new int[]{small2, big2};
        } else         if (left_diff > cross_diff) {
            return new int[]{small1, big1};
        } else {
            return new int[]{min_left, max_right};
        }
    }

    static int[] max_difference(int[] a) {
        return max_diff_range(((int[])(a)), 0, a.length);
    }

    static void main() {
        int[] result = ((int[])(max_difference(((int[])(new int[]{5, 11, 2, 1, 7, 9, 0, 7})))));
        System.out.println(_p(result));
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
