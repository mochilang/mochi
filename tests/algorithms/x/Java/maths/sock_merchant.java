public class Main {

    static int sock_merchant(int[] colors) {
        int[] arr = ((int[])(new int[]{}));
        int i = 0;
        while (i < colors.length) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(colors[i])).toArray()));
            i = i + 1;
        }
        int n = arr.length;
        int a = 0;
        while (a < n) {
            int min_idx = a;
            int b = a + 1;
            while (b < n) {
                if (arr[b] < arr[min_idx]) {
                    min_idx = b;
                }
                b = b + 1;
            }
            int temp = arr[a];
arr[a] = arr[min_idx];
arr[min_idx] = temp;
            a = a + 1;
        }
        int pairs = 0;
        i = 0;
        while (i < n) {
            int count = 1;
            while (i + 1 < n && arr[i] == arr[i + 1]) {
                count = count + 1;
                i = i + 1;
            }
            pairs = pairs + count / 2;
            i = i + 1;
        }
        return pairs;
    }

    static void test_sock_merchant() {
        int[] example1 = ((int[])(new int[]{10, 20, 20, 10, 10, 30, 50, 10, 20}));
        if (sock_merchant(((int[])(example1))) != 3) {
            throw new RuntimeException(String.valueOf("example1 failed"));
        }
        int[] example2 = ((int[])(new int[]{1, 1, 3, 3}));
        if (sock_merchant(((int[])(example2))) != 2) {
            throw new RuntimeException(String.valueOf("example2 failed"));
        }
    }

    static void main() {
        test_sock_merchant();
        int[] example1_1 = ((int[])(new int[]{10, 20, 20, 10, 10, 30, 50, 10, 20}));
        System.out.println(_p(sock_merchant(((int[])(example1_1)))));
        int[] example2_1 = ((int[])(new int[]{1, 1, 3, 3}));
        System.out.println(_p(sock_merchant(((int[])(example2_1)))));
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
