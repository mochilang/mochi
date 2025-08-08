public class Main {
    static class InvResult {
        int[] arr;
        int inv;
        InvResult(int[] arr, int inv) {
            this.arr = arr;
            this.inv = inv;
        }
        InvResult() {}
        @Override public String toString() {
            return String.format("{'arr': %s, 'inv': %s}", String.valueOf(arr), String.valueOf(inv));
        }
    }

    static int[] arr_1 = new int[0];
    static int nbf;
    static InvResult nrec;
    static int nbf2;
    static InvResult nrec2;
    static int nbf3;
    static InvResult nrec3;

    static int[] slice_list(int[] arr, int start, int end) {
        int[] res = ((int[])(new int[]{}));
        int k = start;
        while (k < end) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(arr[k])).toArray()));
            k = k + 1;
        }
        return res;
    }

    static int count_inversions_bf(int[] arr) {
        int n = arr.length;
        int inv = 0;
        int i = 0;
        while (i < n - 1) {
            int j = i + 1;
            while (j < n) {
                if (arr[i] > arr[j]) {
                    inv = inv + 1;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return inv;
    }

    static InvResult count_cross_inversions(int[] p, int[] q) {
        int[] r = ((int[])(new int[]{}));
        int i_1 = 0;
        int j_1 = 0;
        int inv_1 = 0;
        while (i_1 < p.length && j_1 < q.length) {
            if (p[i_1] > q[j_1]) {
                inv_1 = inv_1 + (p.length - i_1);
                r = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(r), java.util.stream.IntStream.of(q[j_1])).toArray()));
                j_1 = j_1 + 1;
            } else {
                r = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(r), java.util.stream.IntStream.of(p[i_1])).toArray()));
                i_1 = i_1 + 1;
            }
        }
        if (i_1 < p.length) {
            r = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(r), java.util.Arrays.stream(slice_list(((int[])(p)), i_1, p.length))).toArray()));
        } else {
            r = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(r), java.util.Arrays.stream(slice_list(((int[])(q)), j_1, q.length))).toArray()));
        }
        return new InvResult(r, inv_1);
    }

    static InvResult count_inversions_recursive(int[] arr) {
        if (arr.length <= 1) {
            return new InvResult(arr, 0);
        }
        int mid = Math.floorDiv(arr.length, 2);
        int[] p = ((int[])(slice_list(((int[])(arr)), 0, mid)));
        int[] q = ((int[])(slice_list(((int[])(arr)), mid, arr.length)));
        InvResult res_p = count_inversions_recursive(((int[])(p)));
        InvResult res_q = count_inversions_recursive(((int[])(q)));
        InvResult res_cross = count_cross_inversions(((int[])(res_p.arr)), ((int[])(res_q.arr)));
        int total = res_p.inv + res_q.inv + res_cross.inv;
        return new InvResult(res_cross.arr, total);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            arr_1 = ((int[])(new int[]{10, 2, 1, 5, 5, 2, 11}));
            nbf = count_inversions_bf(((int[])(arr_1)));
            nrec = count_inversions_recursive(((int[])(arr_1))).inv;
            System.out.println("number of inversions = " + " " + String.valueOf(nbf));
            arr_1 = ((int[])(new int[]{1, 2, 2, 5, 5, 10, 11}));
            nbf2 = count_inversions_bf(((int[])(arr_1)));
            nrec2 = count_inversions_recursive(((int[])(arr_1))).inv;
            System.out.println("number of inversions = " + " " + String.valueOf(nbf2));
            arr_1 = ((int[])(new int[]{}));
            nbf3 = count_inversions_bf(((int[])(arr_1)));
            nrec3 = count_inversions_recursive(((int[])(arr_1))).inv;
            System.out.println("number of inversions = " + " " + String.valueOf(nbf3));
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
