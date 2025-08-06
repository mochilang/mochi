public class Main {

    static int ceil_index(int[] v, int left, int right, int key) {
        int l = left;
        int r = right;
        while (r - l > 1) {
            int middle = (l + r) / 2;
            if (v[middle] >= key) {
                r = middle;
            } else {
                l = middle;
            }
        }
        return r;
    }

    static int longest_increasing_subsequence_length(int[] v) {
        if (v.length == 0) {
            return 0;
        }
        int[] tail = ((int[])(new int[]{}));
        int i = 0;
        while (i < v.length) {
            tail = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(tail), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        int length = 1;
tail[0] = v[0];
        int j = 1;
        while (j < v.length) {
            if (v[j] < tail[0]) {
tail[0] = v[j];
            } else             if (v[j] > tail[length - 1]) {
tail[length] = v[j];
                length = length + 1;
            } else {
                int idx = ceil_index(((int[])(tail)), -1, length - 1, v[j]);
tail[idx] = v[j];
            }
            j = j + 1;
        }
        return length;
    }

    static void main() {
        int[] example1 = ((int[])(new int[]{2, 5, 3, 7, 11, 8, 10, 13, 6}));
        int[] example2 = ((int[])(new int[]{}));
        int[] example3 = ((int[])(new int[]{0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15}));
        int[] example4 = ((int[])(new int[]{5, 4, 3, 2, 1}));
        System.out.println(longest_increasing_subsequence_length(((int[])(example1))));
        System.out.println(longest_increasing_subsequence_length(((int[])(example2))));
        System.out.println(longest_increasing_subsequence_length(((int[])(example3))));
        System.out.println(longest_increasing_subsequence_length(((int[])(example4))));
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
