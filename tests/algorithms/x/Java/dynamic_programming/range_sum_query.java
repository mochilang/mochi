public class Main {
    static class Query {
        int left;
        int right;
        Query(int left, int right) {
            this.left = left;
            this.right = right;
        }
        Query() {}
        @Override public String toString() {
            return String.format("{'left': %s, 'right': %s}", String.valueOf(left), String.valueOf(right));
        }
    }

    static int[] arr1;
    static Query[] queries1;
    static int[] arr2;
    static Query[] queries2;

    static int[] prefix_sum(int[] arr, Query[] queries) {
        int[] dp = ((int[])(new int[]{}));
        int i = 0;
        while (i < arr.length) {
            if (i == 0) {
                dp = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(dp), java.util.stream.IntStream.of(arr[0])).toArray()));
            } else {
                dp = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(dp), java.util.stream.IntStream.of(dp[i - 1] + arr[i])).toArray()));
            }
            i = i + 1;
        }
        int[] result = ((int[])(new int[]{}));
        int j = 0;
        while (j < queries.length) {
            Query q = queries[j];
            int sum = dp[q.right];
            if (q.left > 0) {
                sum = sum - dp[q.left - 1];
            }
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(sum)).toArray()));
            j = j + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            arr1 = ((int[])(new int[]{1, 4, 6, 2, 61, 12}));
            queries1 = ((Query[])(new Query[]{new Query(2, 5), new Query(1, 5), new Query(3, 4)}));
            System.out.println(_p(prefix_sum(((int[])(arr1)), ((Query[])(queries1)))));
            arr2 = ((int[])(new int[]{4, 2, 1, 6, 3}));
            queries2 = ((Query[])(new Query[]{new Query(3, 4), new Query(1, 3), new Query(0, 2)}));
            System.out.println(_p(prefix_sum(((int[])(arr2)), ((Query[])(queries2)))));
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
